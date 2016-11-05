module Component.SuperTimer exposing (Msg (..), Model, init, update, view, subscriptions)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing ( onClick )
--import Time exposing (Time, every, second)
--import Random
import Dict exposing (Dict)
import Component.TaskTimer as TaskTimer
import Component.Many as Many
--import Component.SuperBuzzer as Buzzer

import Updater exposing (converter, Updater, Converter, Interface, toCmd, noReaction)


type alias Model = { timers : Many.Model TaskTimer.Model TaskTimer.Msg
                   , message : String }


type Msg = NoOp
         | UpdaterMsg (Updater Model Msg)


timersC : Converter Msg (Many.Msg TaskTimer.Model TaskTimer.Msg)
timersC = converter
           UpdaterMsg
           { get = Just << .timers
           , set = (\ cm model -> { model | timers = cm } )
           , update = Many.update
           , react = (\ msg _ model ->
                          case msg of
                              Many.Passing id cMsg cModel ->
                                  case cMsg of
                                      TaskTimer.BuzzMessage name description ->
                                          { model | message = name ++ " : " ++ description }
                                          ! []

                                      _ -> model ! []

                              _ -> model ! [] ) }

--

init : (Model, Cmd Msg)
init = { timers = Many.initModel TaskTimer.update TaskTimer.subscriptions
       , message = ""}
    ! [ ]

-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
      NoOp -> model ! []
      UpdaterMsg u -> u model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Sub.map timersC <| Many.subscriptions model.timers ]

-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib
view : Model -> Html Msg
view model =
  div [ ]
      [ div [ style [("height", "420px")]] <|
            List.map (\ (id, timerModel) ->
                          deletableTimer id <|
                          Html.map (timersC << model.timers.converter id) <|
                          TaskTimer.view timerModel)
            (Dict.toList model.timers.objects)

      , div [] [
             Html.map timersC <|
             button [ onClick <| Many.Add TaskTimer.init ] [ text "Add Timer" ] ]
      , h4 [] [ text model.message ]
      ]


deletableTimer : Int -> Html Msg -> Html Msg
deletableTimer id html = div [ style [ ("width", "215px")
                                     , ("float", "left")
                                     , ("height", "320px") ] ]
               [ html
               , Html.map timersC <|
                   button [ onClick <| Many.Delete id ] [ text "Delete" ]
               ]


-- APP
main : Program Never
main =
    Html.program { init = init
                 , update = update
                 , subscriptions = subscriptions
                 , view = view }
