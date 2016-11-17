module Component.Timer exposing (Msg (..), Model, State, init, update, view, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html as Html
import Html.Events exposing ( onClick )
import Time exposing (Time, every, second)
import Random
import Updater exposing (toCmd)

type State = Counting | Buzzing | Setting

type alias Model = { state : State
                   , count : Int
                   , startCount : Int
                   , buzzRed: Bool }

type Msg = Decrement
         | Increment
         | Set Int
         | EverySecond Time
         | Countdown
         | Start
         | Stop
         | Buzz
         | BuzzRedToggle
         | Random

init : (Model, Cmd Msg)
init = { state = Setting
       , count = 0
       , startCount = 10
       , buzzRed = False }
       ! [ toCmd Random ]

-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
      Decrement -> { model | startCount = Basics.max 1 <|model.startCount - 1 } ! []
      Increment -> { model | startCount = model.startCount + 1 } ! []
      Set n -> { model | startCount = n } ! []
      EverySecond _ -> model ! [ case model.state of
                                     Buzzing -> toCmd BuzzRedToggle
                                     Counting -> toCmd Countdown
                                     _ -> Cmd.none ]
      Countdown -> let c = model.count - 1 in
                   { model | count = c } ! [ if c == 0 then
                                                 toCmd Buzz
                                             else
                                                 Cmd.none ]
      Start -> { model | state = Counting
               , count = model.startCount } ! []
      Stop -> { model | state = Setting } ! []
      Buzz -> { model | state = Buzzing } ! []
      BuzzRedToggle -> { model | buzzRed = not model.buzzRed } ! []
      Random -> model ! [ Random.generate Set <| Random.int  1 30 ]

subscriptions : Model -> Sub Msg
subscriptions model =
    if model.state == Buzzing || model.state == Counting then
        every second EverySecond
    else
        Sub.none

-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib
view : Model -> Html Msg
view model =
    div [ style [ ("border", "1px solid #aaa")
                , ("margin", "10px")
                , ("width", "130px")
                , ("padding", "10px")
                , ("background-color", if isBuzzRed model then
                                           "red"
                                       else
                                           "white" ) ] ]
      [ if model.state == Setting then
            div [ class "counter-setting" ]
                [ text <| toString model.startCount ]
        else
            div [ class "counter-counting" ]
                [ text <| toString model.count ]
      , case model.state of
            Setting ->
                div [] [ button [ onClick Increment ] [ text "+" ]
                       , button [ onClick Decrement ] [ text "-" ]
                       , button [ onClick Start ] [ text "Start" ] ]
            _ ->
                div [] [ button [ onClick Stop ] [ text "Stop / Reset" ] ]
      ]

isBuzzRed : Model -> Bool
isBuzzRed model = model.state == Buzzing && model.buzzRed

-- APP
main : Program Never Model Msg
main =
    Html.program { init = init
                 , update = update
                 , subscriptions = subscriptions
                 , view = view }
