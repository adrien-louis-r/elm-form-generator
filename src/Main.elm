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
  { formDefinition: FormDefinition.Model
  , nextID: ID
  }

init : (Model, Cmd Msg)
init =
  ( { formDefinition = FormDefinition.init
    , nextID = 1
    }
    , Cmd.none
  )


-- UPDATE
type Msg
  = RemoveField ID Field.Msg
  | UpdateDefinition FormDefinition.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    RemoveField id msg ->
      ( model, Cmd.none )

    UpdateDefinition msg ->
      ( { model | formDefinition = FormDefinition.update msg model.formDefinition }, Cmd.none)


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW
view : Model -> Html Msg
view model =
  div [ style [ ("display", "flex"), ("height", "100%") ] ]
    [
      div [ style [ ("flex", "1"), ("border-right", "1px solid black") ] ]
        (List.map viewField (List.reverse model.formDefinition.result))
    , div [ style [ ("flex", "1") ] ]
        [ FormDefinition.view model.formDefinition |> Html.map UpdateDefinition ]
    ]

viewField : (ID, Field.Model) -> Html Msg
viewField (id, model) =
  Html.map (RemoveField id) (Field.view model)
