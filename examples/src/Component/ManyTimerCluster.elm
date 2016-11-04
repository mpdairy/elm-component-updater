module Component.ManualTimerCluster exposing (Msg (..), Model, init, update, view)
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


type alias Model = { timers : Many.Model TaskTimer.Model TaskTimer.Msg }


type Msg = AddTimer
         | UpdaterMsg (Updater Model Msg)


timersC : Converter Msg (Many.Msg TaskTimer.Model TaskTimer.Msg)
timersC = converter
           UpdaterMsg
           { get = Just << .timers
           , set = (\ cm model -> { model | timers = cm } )
           , update = Many.update
           , react = noReaction }

--

init : (Model, Cmd Msg)
init = { timers = Many.initModel TaskTimer.update TaskTimer.subscriptions }
    ! [ ]

-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
      AddTimer -> model ! [ Cmd.map timersC <| toCmd <| Many.Add TaskTimer.init ]
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
                          Html.map (timersC << model.timers.converter id) <|
                          TaskTimer.view timerModel)
            (Dict.toList model.timers.objects)

      , div [] [ button [ onClick AddTimer ] [ text "Add Timer" ] ]
      ]

      {-
deletableTimer : Int -> Html Msg -> Html Msg
deletableTimer id html = div [ style [ ("width", "215px")
                                     , ("float", "left")
                                     , ("height", "320px") ] ]
               [ html
               , button [ onClick <| DeleteTimer id ] [ text "Delete" ]
               ]
-}

-- APP
main : Program Never
main =
    Html.program { init = init
                 , update = update
                 , subscriptions = subscriptions
                 , view = view }
