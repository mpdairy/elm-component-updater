module Component.ManualTimerCluster exposing (Msg (..), Model, init, update, view, subscriptions)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing ( onClick )

import Component.TaskTimer as TaskTimer
import Component.Many as Many

import Updater exposing (converter, Updater, Converter, Interface, toCmd, noReaction)

type alias TimersModel = Many.Model TaskTimer.Model TaskTimer.Msg

type alias TimersMsg = Many.Msg TaskTimer.Model TaskTimer.Msg


type alias Model = { timers : TimersModel }

type Msg = NoOp
         | UpdaterMsg (Updater Model Msg)


timersC : Converter Msg TimersMsg
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
      NoOp -> model ! []
      UpdaterMsg u -> u model

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Sub.map timersC <| Many.subscriptions model.timers ]

-- VIEW
view : Model -> Html Msg
view model =
    div []
        [ Html.map timersC <| viewTimers model.timers ]

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
