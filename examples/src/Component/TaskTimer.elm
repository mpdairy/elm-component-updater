module Component.TaskTimer exposing (Msg (..), Model, init, update, view, subscriptions)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html as Html
import Component.Timer as Timer
import Component.EditableLabel as Label

import Updater exposing (converter, Updater, Converter, Interface, toCmd, noReaction)


type alias Model = { timer : Timer.Model
                   , name : Label.Model
                   , task : Label.Model
                   , buzzCount : Int }


type Msg = Reset
         | BuzzMessage String String
         | UpdaterMsg (Updater Model Msg)


nameC : Converter Msg Label.Msg
nameC = converter
        UpdaterMsg
        { get = Just << .name
        , set = (\ cm m -> { m | name = cm } )
        , update = Label.update
        , react = noReaction }

taskC : Converter Msg Label.Msg
taskC = converter
        UpdaterMsg
        { get = Just << .task
        , set = (\ cm m -> { m | task = cm } )
        , update = Label.update
        , react = noReaction }

timerC : Converter Msg Timer.Msg
timerC = converter
         UpdaterMsg
         { get = Just << .timer
         , set = (\ cm m -> { m | timer = cm } )
         , update = Timer.update
         , react = (\ tmsg _ model ->
                         case tmsg of
                             Timer.Buzz ->
                                 { model | buzzCount = model.buzzCount + 1 }
                                 ! [ toCmd <| BuzzMessage model.name.text model.task.text ]
                             _ -> model ! [] ) }

--

init : (Model, Cmd Msg)
init = let ( tmodel, tcmd ) = Timer.init
       in
           { timer = tmodel
           , task = Label.initModel "Enter a task description here."
           , name = Label.initModel "Timer Name"
           , buzzCount = 0 }
    ! [ Cmd.map timerC tcmd ]

-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
      Reset -> model ! [ Cmd.map timerC <| toCmd Timer.Stop ]

      -- just an information message for parents
      BuzzMessage _ _ -> model ! []

      UpdaterMsg u -> u model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map timerC <| Timer.subscriptions model.timer

-- VIEW
view : Model -> Html Msg
view model =
  div [ style [ ("border", "1px solid #aaa"), ("margin", "10px")
              , ("width", "175px"), ("padding", "10px")
              , ("float", "left"), ("height", "320px")] ]
      [ Html.map timerC <| Timer.view model.timer
      , div [] [ h3 [] [text "Name:"]
               , Html.map nameC <| Label.view model.name ]
      , div [] [ h3 [] [text "Description:"]
               , Html.map taskC <| Label.view model.task ]

      , h4 [] [ text <| "Times Buzzed: " ++ toString model.buzzCount ]
      ]

-- APP
main : Program Never Model Msg
main =
    Html.program { init = init
                 , update = update
                 , subscriptions = subscriptions
                 , view = view }
