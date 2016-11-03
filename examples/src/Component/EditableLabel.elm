module Component.EditableLabel exposing (Msg (..), Model, initModel, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing ( onClick, onInput )

type Msg = Input String
         | Set
         | Cancel
         | Edit

type alias Model = { text : String
                   , editText : String
                   , editing : Bool }

initModel : String -> Model
initModel initialText = { text = initialText
                        , editText = initialText
                        , editing = False }

-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
      Input s -> { model | editText = s } ! []
      Edit -> { model | editing = True } ! []
      Set -> { model | text = model.editText
             , editing = False } ! []
      Cancel -> { model | editText = model.text
                , editing = False } ! []

-- VIEW
view : Model -> Html Msg
view model =
  div [ class "editable-label" ] <|
       if model.editing then
            [ input [ value model.editText, onInput Input ] []
            , button [ onClick Set, class "set-button" ] [ text "Set" ]
            , button [ onClick Cancel, class "cancel-button" ] [ text "Cancel" ] ]
        else
            [ span [ ] [ text model.text ]
            , button [ onClick Edit, class "edit-button" ] [ text "Edit" ] ]

-- APP
main : Program Never
main =
    Html.program { init = ( initModel "", Cmd.none )
                 , update = update
                 , subscriptions = always Sub.none
                 , view = view }
