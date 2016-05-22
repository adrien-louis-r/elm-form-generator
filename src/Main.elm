import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task exposing (..)
import Field exposing (..)
import FormDefinition exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL
type alias ID = Int
type alias Model =
  { fields: List ( ID, Field.Model )
  , formDefinition: FormDefinition.Model
  , nextID: ID
  }

init : (Model, Cmd Msg)
init =
  ( { fields = [(0 , Field.initText 0)]
    , formDefinition = FormDefinition.init
    , nextID = 1
    }
    , Cmd.none
  )


-- UPDATE
type Msg
  = AddTextField
  | AddNumberField
  | RemoveField ID Field.Msg
  | UpdateDefinition FormDefinition.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddTextField ->
      { model |
        fields = ( model.nextID, Field.initText model.nextID ) :: model.fields
      , nextID = model.nextID + 1
      } |> (update AddNumberField)

    AddNumberField ->
      ( { model |
          fields = ( model.nextID, Field.initNumber model.nextID ) :: model.fields
        , nextID = model.nextID + 1
        }
      , Cmd.none
      )

    RemoveField id msg ->
      let
        updateField (fieldID, fieldModel) =
          case (fieldID == id, Field.update msg fieldModel) of
            (False, _) ->
              Just (fieldID, fieldModel)
            (True, (newFieldModel, Just Field.Remove)) ->
              Nothing
            (True, (newFieldModel, _)) ->
              Just (fieldID, newFieldModel)
      in
        ( { model | fields = List.filterMap updateField model.fields }, Cmd.none )

    UpdateDefinition msg ->
      ( { model | formDefinition = FormDefinition.update msg model.formDefinition }, Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW
view : Model -> Html Msg
view model =
  let
    addText = button [ onClick AddTextField ] [ text "Add text" ]
    addNumber = button [ onClick AddNumberField ] [ text "Add number" ]
  in
    div [ style [ ("display", "flex") ] ]
      [
        div [ style [ ("flex", "1") ] ]
          (addText :: addNumber :: List.map viewField (List.reverse model.fields))
      , div [ style [ ("flex", "1") ] ] [FormDefinition.view model.formDefinition |> Html.map UpdateDefinition]
      ]

viewField : (ID, Field.Model) -> Html Msg
viewField (id, model) =
  Html.map (RemoveField id) (Field.view model)
