module Component.SuperBuzzer exposing (Msg (..), Model, initModel, update, view, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing ( onClick )
import Time exposing (Time, every, millisecond, second)
import Updater exposing (toCmd)

type Msg = Tick Time
         | Start
         | Stop

type alias Model = { maxHeight : Float
                   , maxWidth : Float
                   , percent : Float
                   , cycleTime : Time
                   , buzzing : Bool
                   , growing : Bool }

initModel : Float -> Float -> Time -> Model
initModel maxWidth maxHeight cycleTime =
    { maxHeight = maxHeight
    , maxWidth = maxWidth
    , percent = 0
    , cycleTime = cycleTime
    , buzzing = False
    , growing = True }

-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
      Start -> { model | buzzing = True
               , growing = True } ! []
      Stop -> { model | buzzing = False } ! []
      Tick _ ->
          let p = model.percent
                  + (if model.growing then
                         1.0
                     else
                         -1.0 ) * ((50 * millisecond) / model.cycleTime) * 100
          in
              { model
                  | percent = Basics.max 0 <| Basics.min 100 p
                  , growing = (if p > 100 || p < 0 then not else identity)
                              model.growing }
              ! []


-- VIEW
view : Model -> Html Msg
view model =
    div [ style [ ("width", px model.maxWidth)
                , ("height", px model.maxHeight)
                , ("padding-top", px <|
                       ((100 - model.percent) / 100 * model.maxHeight) / 2)]
        , onClick Stop ]
        [ div [ style [ ("background-color", if model.buzzing then "red" else "white")
                      , ("width", percent model.percent)
                      , ("height", percent model.percent)
                      , ("margin", "auto auto")] ]
              [] ]

px : Float -> String
px n = (toString n) ++ "px"

percent : Float -> String
percent p = (toString p) ++ "%"

subscriptions : Model -> Sub Msg
subscriptions model =
    if model.buzzing then
        every (millisecond * 50) Tick
    else
        Sub.none

-- APP
main : Program Never
main =
    Html.program { init = ( initModel 300 200 (second * 0.5), toCmd Start )
                 , update = update
                 , subscriptions = subscriptions
                 , view = view }
