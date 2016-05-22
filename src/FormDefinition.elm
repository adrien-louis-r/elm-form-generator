module FormDefinition exposing (..)

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import FieldDefinition


-- MODEL
type alias Model = List (Int, FieldDefinition.Model)

init : Model
init = [(0, FieldDefinition.init)]


-- UPDATE
type Msg
  = Modify Int FieldDefinition.Msg


update : Msg -> Model -> Model
update msg model =
  case msg of
    Modify id msg ->
      let
        updateField (fieldID, fieldDefinition) =
          case (fieldID == id, FieldDefinition.update msg fieldDefinition) of
            (False, _) ->
              Just (fieldID, fieldDefinition)
            (True, newFieldModel) ->
              Just (fieldID, newFieldModel)
      in
        List.filterMap updateField model


-- VIEW
view : Model -> Html Msg
view model =
  div []
    (List.map fieldDefinitionView model)

fieldDefinitionView: (Int, FieldDefinition.Model) -> Html Msg
fieldDefinitionView (id, fieldDefinition) =
  div []
    [Html.map (Modify id) (FieldDefinition.view fieldDefinition)]
