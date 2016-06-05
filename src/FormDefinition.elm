module FormDefinition exposing (..)

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import FieldDefinition
import Field


-- MODEL
type alias Model =
  { fields: List (Int, FieldDefinition.Model)
  , nextId: Int
  , result: List (Int, Field.Model)
  }

init : Model
init =
  { fields = []
  , nextId = 0
  , result = []
  }


-- UPDATE
type Msg
  = Modify Int FieldDefinition.Msg
  | AddDefinition
  | AddDefinitionNumber


update : Msg -> Model -> Model
update msg model =
  case msg of
    Modify id msg ->
      let
        updateField (fieldID, fieldDefinition) =
          case (fieldID == id, FieldDefinition.update msg fieldDefinition) of
            (False, _) ->
              Just (fieldID, fieldDefinition)
            (True, (newFieldModel, Just FieldDefinition.Remove)) ->
              Nothing
            (True, (newFieldModel, _)) ->
              Just (fieldID, newFieldModel)

        newFields = List.filterMap updateField model.fields
      in
        { model | fields = newFields, result = formDefinitionToform newFields }

    AddDefinition ->
      let
        newFields = (model.nextId, FieldDefinition.init FieldDefinition.Text model.nextId) :: model.fields
      in
        { model |
          fields = newFields
        , nextId = model.nextId + 1
        , result = formDefinitionToform newFields
        }

    AddDefinitionNumber ->
      let
        newFields = (model.nextId, FieldDefinition.init FieldDefinition.Number model.nextId) :: model.fields
      in
        { model |
          fields = newFields
        , nextId = model.nextId + 1
        , result = formDefinitionToform newFields
        }


formDefinitionToform : List (Int, FieldDefinition.Model) -> List (Int, Field.Model)
formDefinitionToform fields =
  let
    transform (fieldID, fieldDefinition) =
      case (fieldID, fieldDefinition) of
        (_, {label, id, type'}) ->
          case type' of
            FieldDefinition.Text ->
              (fieldID, Field.initText label.value id.value)

            FieldDefinition.Number ->
              (fieldID, Field.initNumber label.value id.value)
  in
    List.map transform fields


-- VIEW
view : Model -> Html Msg
view model =
  let
    addDefinition = button [ onClick AddDefinition ] [ text "Add new Field" ]
    addDefinitionNumber = button [ onClick AddDefinitionNumber ] [ text "Add new number Field" ]
  in
    div []
      (addDefinition :: addDefinitionNumber :: List.map fieldDefinitionView model.fields)

fieldDefinitionView: (Int, FieldDefinition.Model) -> Html Msg
fieldDefinitionView (id, fieldDefinition) =
  div []
    [ div [] [ text ("Field " ++ toString id) ]
    , Html.map (Modify id) (FieldDefinition.view fieldDefinition)
    ]
