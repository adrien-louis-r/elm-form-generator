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
  { fields = [(0, FieldDefinition.init 0)]
  , nextId = 1
  , result = []
  }


-- UPDATE
type Msg
  = Modify Int FieldDefinition.Msg
  | AddDefinition
  | GenerateForm


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
      in
        { model | fields = List.filterMap updateField model.fields }

    AddDefinition ->
      { model |
        fields = (model.nextId, FieldDefinition.init model.nextId) :: model.fields
      , nextId = model.nextId + 1
      }

    GenerateForm ->
      { model | result = formDefinitionToform model.fields }


formDefinitionToform : List (Int, FieldDefinition.Model) -> List (Int, Field.Model)
formDefinitionToform fields =
  let
    transform (fieldID, fieldDefinition) =
      case (fieldID, fieldDefinition) of
        (_, fieldDefinition) ->
          (fieldID, Field.initText fieldDefinition.label.value fieldDefinition.id.value)
  in
    List.map transform fields


-- VIEW
view : Model -> Html Msg
view model =
  let
    addDefinition = button [ onClick AddDefinition ] [ text "Add new Field" ]
    generateForm = button [ onClick GenerateForm ] [ text "Generate form" ]
  in
    div []
      (generateForm :: addDefinition :: List.map fieldDefinitionView model.fields)

fieldDefinitionView: (Int, FieldDefinition.Model) -> Html Msg
fieldDefinitionView (id, fieldDefinition) =
  div []
    [ div [] [ text ("Field " ++ toString id) ]
    , Html.map (Modify id) (FieldDefinition.view fieldDefinition)
    ]
