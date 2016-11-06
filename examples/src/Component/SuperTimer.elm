module Component.SuperTimer exposing (Msg (..), Model, init, update, view, subscriptions)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing ( onClick )
import Time exposing (second)
import Dict exposing (Dict)
import Updater exposing (converter, Updater, Converter, Interface, toCmd, noReaction)

import Component.TaskTimer as TaskTimer
import Component.Many as Many
import Component.SuperBuzzer as Buzzer

when : Bool -> a -> Maybe a
when b x = if b then Just x else Nothing

type alias TimersModel = Many.Model TaskTimer.Model TaskTimer.Msg

type alias TimersMsg = Many.Msg TaskTimer.Model TaskTimer.Msg


type alias Model = { timers : TimersModel
                   , buzzer : Buzzer.Model
                   , buzzedTimers : Dict Int (String, String) }

type Msg = Buzz Int String String
         | UnBuzz
         | UpdaterMsg (Updater Model Msg)


timersC : Converter Msg TimersMsg
timersC = converter
          UpdaterMsg
          { get = Just << .timers
          , set = \ cm model -> { model | timers = cm }
          , update = Many.update
          , react = \ mMsg _ model ->
                         case mMsg of
                             Many.From id tMsg _ ->
                                 case tMsg of
                                     TaskTimer.BuzzMessage name description ->
                                         model
                                         ! [ toCmd <| Buzz id name description ]
                                     _ -> model ! []
                             _ -> model ! [] }

buzzerC : Converter Msg Buzzer.Msg
buzzerC = converter
          UpdaterMsg
          { get = Just << .buzzer
          , set = \ cm m -> { m | buzzer = cm}
          , update = Buzzer.update
          , react = \ bMsg _ model ->
                    case bMsg of
                        Buzzer.Stop ->
                            model ! [ toCmd UnBuzz ]
                        _ ->
                            model ! [] }

init : (Model, Cmd Msg)
init = { timers = Many.initModel TaskTimer.update TaskTimer.subscriptions
       , buzzer = Buzzer.initModel 500 300 (second * 0.5)
       , buzzedTimers = Dict.empty }
    ! [ ]

-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
      Buzz id name desc -> { model | buzzedTimers =
                                 Dict.insert id (name, desc)
                                 model.buzzedTimers }
                           ! [ Cmd.map buzzerC <| toCmd Buzzer.Start ]
      UnBuzz -> { model | buzzedTimers = Dict.empty }
                ! (List.map
                       (\ id -> Cmd.map timersC <| toCmd <|
                            Many.SendTo id TaskTimer.Reset )
                       (Dict.keys model.buzzedTimers))
      UpdaterMsg u -> u model

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Sub.map timersC <| Many.subscriptions model.timers
              , Sub.map buzzerC <| Buzzer.subscriptions model.buzzer ]

-- VIEW
view : Model -> Html Msg
view model =
    div [] <|
        List.filterMap identity <|
        [ Just <| Html.map timersC <| viewTimers model.timers
        , when model.buzzer.buzzing <|
            div []
                [ text "Click to clear buzzer."
                , div [style [("height", "400px")]]
                    [ div [ style [("width", "500px"), ("float", "left")]]
                          [ Html.map buzzerC <| Buzzer.view model.buzzer ]
                    , div [ style [("width", "500px"), ("float", "left")] ]
                        [ viewBuzzedTasks model.buzzedTimers ] ] ]
        ]

viewBuzzedTasks : Dict Int (String, String) -> Html Msg
viewBuzzedTasks bt =
    ul [] <|
        List.map (\ (name, desc) -> li [] [ text <| name ++ " : " ++ desc ] ) <|
            Dict.values bt


viewTimers : TimersModel -> Html TimersMsg
viewTimers timers =
    div [ class "timers" ]
        [ div [ style [("height", "420px")]] <|

              timers.viewAll
              (\ id timer conv -> Just <|
                   div [ style [ ("width", "215px")
                               , ("float", "left")
                               , ("height", "320px") ] ]
                   [ conv <| TaskTimer.view timer
                   , button [ onClick <| Many.Delete id ] [ text "Delete" ] ])

        , div [] [ button [ onClick <| Many.Add TaskTimer.init ] [ text "Add Timer" ]]]

-- APP
main : Program Never
main =
    Html.program { init = init
                 , update = update
                 , subscriptions = subscriptions
                 , view = view }
