module Component.Cluster exposing (Msg (..), Model, init, update, view)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing ( onClick )
--import Time exposing (Time, every, second)
--import Random
import Component.Timer as Timer
import Component.Nametag as Nametag

import Updater exposing (converter, Updater, Converter, Interface, toCmd)


--
--
type Msg = NoOp
         | ResetAll
         | UpdaterMsg (Updater Model Msg)


t1uconv : Converter Msg Timer.Msg
t1uconv = converter
          UpdaterMsg
          { get = Just << .timer1
          , set = (\ cm m -> { m | timer1 = cm } )
          , update = Timer.update
          , react = (\ tmsg tmodel model ->
                         case tmsg of
                             Timer.Set n ->
                                 { model | recordedTime = n } ! []
                             _ -> model ! [] ) }
--
t2uconv : Converter Msg Timer.Msg
t2uconv = converter
          UpdaterMsg
          { get = Just << .timer2
          , set = (\ cm m -> { m | timer2 = cm } )
          , update = Timer.update
          , react = (\ tmsg tmodel model ->
                         case tmsg of
                             Timer.Set n ->
                                 { model | recordedTime = n } ! []
                             _ -> model ! [] ) }
--
t3uconv : Converter Msg Timer.Msg
t3uconv = converter
          UpdaterMsg
          { get = Just << .timer3
          , set = (\ cm m -> { m | timer3 = cm } )
          , update = Timer.update
          , react = (\ tmsg tmodel model ->
                         case tmsg of
                             Timer.Set n ->
                                 { model | recordedTime = n } ! []
                             Timer.Reset ->
                                 model ! [ Cmd.map t1uconv <| toCmd (Timer.Set 666)
                                         , Cmd.map t2uconv <| toCmd (Timer.Set 999)]
                             _ -> model ! [] ) }
--
{-
timerC : Int -> Converter Msg Timer.Msg
timerC n = converter
           UpdaterMsg
           { get = (\ model -> Dict.get n model.timers )
           , set = (\ cModel pModel -> { pModel | timers =
                                             Dict.insert n cModel pModel.timers } )
           , update = Timer.update
           , react = noReaction }
-}
--
nt1uconv : Converter Msg Nametag.Msg
nt1uconv = converter
          UpdaterMsg
          { get = Just << .nametag1
          , set = (\ cm m -> { m | nametag1 = cm } )
          , update = Nametag.update
          , react = (\ cmsg cmodel model ->
                         case cmsg of
                             _ -> model ! [] ) }
--
type alias Model = { timer1 : Timer.Model
                   , timer2 : Timer.Model
                   , timer3 : Timer.Model
                   , recordedTime : Int
                   , nametag1 : Nametag.Model }

init : (Model, Cmd Msg)
init = let ( tmodel, tmsg ) = Timer.init
           ( ntmodel, ntmsg ) = Nametag.init
       in
           { timer1 = tmodel
           , timer2 = tmodel
           , timer3 = tmodel
           , recordedTime = 0
           , nametag1 = ntmodel }
    ! [ Cmd.map t1uconv tmsg
      , Cmd.map t2uconv tmsg
      , Cmd.map t3uconv tmsg ]

-- APP
main : Program Never
main =
    Html.program { init = init
                 , update = update
                 , subscriptions = subscriptions
                 , view = view }

-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
      NoOp -> model ! []
      ResetAll -> model ! [ Cmd.map t1uconv <| toCmd Timer.Reset
                          , Cmd.map t2uconv <| toCmd Timer.Reset
                          , Cmd.map t3uconv <| toCmd Timer.Reset ]

      UpdaterMsg u -> u model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Sub.map t1uconv <| Timer.subscriptions model.timer1
              , Sub.map t2uconv <| Timer.subscriptions model.timer2
              , Sub.map t3uconv <| Timer.subscriptions model.timer3 ]

-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib
view : Model -> Html Msg
view model =
  div [ style [ ("border", "1px solid #aaa"), ("margin", "10px")
              , ("width", "175px"), ("padding", "10px")] ]
      [ Html.map t1uconv <| Timer.view model.timer1
      , Html.map t2uconv <| Timer.view model.timer2
      , Html.map t3uconv <| Timer.view model.timer3
      , div [] [ text <| "Last Set Time: " ++ toString model.recordedTime ]
      , button [ onClick ResetAll ] [ text "Reset All" ]
      , Html.map nt1uconv <| Nametag.view model.nametag1 ]
