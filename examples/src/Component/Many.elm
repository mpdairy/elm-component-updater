module Component.Many exposing (Msg (Add, Delete, From, SendTo), Model, initModel, update, subscriptions)

import Html exposing (Html)
import Html.App as Html
import Dict exposing (Dict)
import List

import Updater exposing (converter, Updater, Converter, Interface, toCmd, noReaction)

type alias Model cModel cMsg =
    { objects : Dict Int cModel
    , newID : Int
    , converter : (Int -> Converter (Msg cModel cMsg) cMsg)
    , objUpdate : ( cMsg -> cModel -> ( cModel, Cmd cMsg ))
    , objSubs : ( cModel -> Sub cMsg )
    , viewAll : ( Int -> cModel -> ( Html cMsg -> Html (Msg cModel cMsg) )
                -> Maybe (Html (Msg cModel cMsg)) )
              -> List (Html (Msg cModel cMsg)) }

type Msg cModel cMsg = Add (cModel, Cmd cMsg)
                     | Delete Int
                     | From Int cMsg cModel
                     | SendTo Int cMsg
                     | UpdaterMsg (Updater (Model cModel cMsg) (Msg cModel cMsg))

objectC : (cMsg -> cModel -> (cModel, Cmd cMsg)) -> Int
        -> Converter (Msg cModel cMsg) cMsg
objectC objUpdate n =
    converter
        UpdaterMsg
        { get = (\ model -> Dict.get n model.objects)
        , set = (\ object model ->
                     let newObjects = Dict.insert n object model.objects in
                     { model | objects = newObjects
                     , viewAll = mapper objUpdate newObjects} )
        , update = objUpdate
        , react = (\ cMsg cModel model -> model ! [ toCmd <| From n cMsg cModel ] ) }
--
mapper : ( cMsg -> cModel -> ( cModel, Cmd cMsg ))
       -> Dict Int cModel
       -> ( Int -> cModel -> ( Html cMsg -> Html (Msg cModel cMsg) )
          -> Maybe ( Html ( Msg cModel cMsg ) ) )
       -> List (Html (Msg cModel cMsg))
mapper objUpdate objects f =
    List.filterMap 
        (\ (id, cModel) ->
             f id cModel (Html.map <| objectC objUpdate id)) <|
        Dict.toList objects

--
initModel : ( cMsg -> cModel -> ( cModel, Cmd cMsg ) )
          -> ( cModel -> Sub cMsg )
          -> Model cModel cMsg
initModel objUpdate objSubs = { objects = Dict.empty
                              , newID = 0
                              , converter = objectC objUpdate
                              , objUpdate = objUpdate
                              , objSubs = objSubs
                              , viewAll = mapper objUpdate Dict.empty }

-- UPDATE
update : Msg cModel cMsg -> Model cModel cMsg
       -> ( Model cModel cMsg, Cmd (Msg cModel cMsg) )
update msg model =
  case msg of
      Add (objectModel, objectCmd) ->
          let newObjects = Dict.insert model.newID objectModel model.objects in
          { model | objects = newObjects
          , viewAll = mapper model.objUpdate newObjects
          , newID = model.newID + 1 }
          ! [ Cmd.map (objectC model.objUpdate model.newID) <| objectCmd ]

      Delete id ->
          let newObjects = Dict.remove id model.objects in
          { model | objects = newObjects
          , viewAll = mapper model.objUpdate newObjects } ! []

      --Passes object messages along to parent
      From id cMsg cModel -> model ! []

      SendTo id cMsg -> model ! [ Cmd.map (objectC model.objUpdate id) <| toCmd cMsg ]

      UpdaterMsg u -> u model


subscriptions : Model cModel cMsg -> Sub (Msg cModel cMsg)
subscriptions model =
    Sub.batch <| List.map (\ (id, objectModel) ->
                              Sub.map (objectC model.objUpdate id) <|
                               model.objSubs objectModel )
                    (Dict.toList model.objects)

