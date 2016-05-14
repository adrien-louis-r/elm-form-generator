import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Field exposing (..)

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
  , nextID: ID
  }

init : (Model, Cmd Msg)
init =
  ( { fields =
      [ ( 0
        , Field.init 0
        )
      ]
    , nextID = 1
    }
    , Cmd.none
  )


-- UPDATE
type Msg
  = AddField
  | RemoveField ID Field.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddField ->
      ( { model |
          fields = ( model.nextID, Field.init model.nextID ) :: model.fields
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

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW
view : Model -> Html Msg
view model =
  let add = button [ onClick AddField ] [ text "Add" ]
  in
    div []
      (add :: List.map viewField (List.reverse model.fields))

viewField : (ID, Field.Model) -> Html Msg
viewField (id, model) =
  Html.map (RemoveField id) (Field.view model)
