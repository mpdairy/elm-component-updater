module Component.Many exposing (Msg (..), Model, initModel, update, subscriptions)

import Dict exposing (Dict)

import Updater exposing (converter, Updater, Converter, Interface, toCmd, noReaction)


type alias Model cModel cMsg =
    { objects : Dict Int cModel
    , newID : Int
    , converter : (Int -> Converter (Msg cModel cMsg) cMsg)
    , objUpdate : ( cMsg -> cModel -> ( cModel, Cmd cMsg ))
    , objSubs : ( cModel -> Sub cMsg )}

type Msg cModel cMsg = Add (cModel, Cmd cMsg)
                     | Delete Int
                     | UpdaterMsg (Updater (Model cModel cMsg) (Msg cModel cMsg))

objectC : (cMsg -> cModel -> (cModel, Cmd cMsg)) -> Int
        -> Converter (Msg cModel cMsg) cMsg
objectC objUpdate n =
    converter
        UpdaterMsg
        { get = (\ model -> Dict.get n model.objects)
        , set = (\ object model -> { model | objects = Dict.insert n object model.objects } )
        , update = objUpdate
        , react = noReaction }
--

initModel : ( cMsg -> cModel -> ( cModel, Cmd cMsg ) )
          -> ( cModel -> Sub cMsg )
          -> Model cModel cMsg
initModel objUpdate objSubs = { objects = Dict.empty
                              , newID = 0
                              , converter = objectC objUpdate
                              , objUpdate = objUpdate
                              , objSubs = objSubs }

-- UPDATE
update : Msg cModel cMsg -> Model cModel cMsg
       -> ( Model cModel cMsg, Cmd (Msg cModel cMsg) )
update msg model =
  case msg of
      Add (objectModel, objectCmd) ->
          { model | objects = Dict.insert model.newID objectModel model.objects
          , newID = model.newID + 1 }
          ! [ Cmd.map (objectC model.objUpdate model.newID) <| objectCmd ]

      Delete id -> { model | objects = Dict.remove id model.objects } ! []

      UpdaterMsg u -> u model


subscriptions : Model cModel cMsg -> Sub (Msg cModel cMsg)
subscriptions model =
    Sub.batch <| List.map (\ (id, objectModel) ->
                              Sub.map (objectC model.objUpdate id) <|
                               model.objSubs objectModel )
                    (Dict.toList model.objects)

