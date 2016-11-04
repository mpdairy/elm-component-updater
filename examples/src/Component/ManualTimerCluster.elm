module Component.ManualTimerCluster exposing (Msg (..), Model, init, update, view)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing ( onClick )
--import Time exposing (Time, every, second)
--import Random
import Dict exposing (Dict)
import Component.TaskTimer as TaskTimer
--import Component.SuperBuzzer as Buzzer

import Updater exposing (converter, Updater, Converter, Interface, toCmd, noReaction)


type alias Model = { timers : Dict Int TaskTimer.Model
                   , newID : Int }


type Msg = AddTimer (TaskTimer.Model, Cmd TaskTimer.Msg)
         | DeleteTimer Int
         | UpdaterMsg (Updater Model Msg)


timerC : Int -> Converter Msg TaskTimer.Msg
timerC n = converter
           UpdaterMsg
           { get = (\ model -> Dict.get n model.timers)
           , set = (\ timer model -> { model | timers = Dict.insert n timer model.timers } )
           , update = TaskTimer.update
           , react = noReaction }

--

init : (Model, Cmd Msg)
init = { timers = Dict.empty
       , newID = 0 }
    ! [ ]

-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
      AddTimer (timerModel, timerCmd) ->
          { model | timers = Dict.insert model.newID timerModel model.timers
          , newID = model.newID + 1 }
          ! [ Cmd.map (timerC model.newID) <| timerCmd ]

      DeleteTimer id -> { model | timers = Dict.remove id model.timers } ! []

      UpdaterMsg u -> u model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <| List.map (\ (id, timerModel) ->
                              Sub.map (timerC id) <| TaskTimer.subscriptions timerModel )
                    (Dict.toList model.timers)

-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib
view : Model -> Html Msg
view model =
  div [ ]
      [ div [ style [("height", "420px")]] <|
            List.map (\ (id, timerModel) ->
                          deletableTimer id <|
                          Html.map (timerC id) <| TaskTimer.view timerModel)
            (Dict.toList model.timers)
      , div [] [ button [ onClick <| AddTimer TaskTimer.init ] [ text "Add Timer" ] ]
      ]

deletableTimer : Int -> Html Msg -> Html Msg
deletableTimer id html = div [ style [ ("width", "215px")
                                     , ("float", "left")
                                     , ("height", "320px") ] ]
               [ html
               , button [ onClick <| DeleteTimer id ] [ text "Delete" ]
               ]

-- APP
main : Program Never
main =
    Html.program { init = init
                 , update = update
                 , subscriptions = subscriptions
                 , view = view }
