module Field exposing (..)
import Html.App
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import TextField

-- MODEL
type FieldType
  = Text
  | Number
  | Select
  | Radio
  | Checkbox

type alias Model =
  { type': FieldType
  , field: TextField.Model
  }

init : Int -> Model
init index =
  { type' = Text
  , field = TextField.init index
  }


-- UPDATE
type Dispatch = Remove

type Msg
  = RemoveSelf
  | Modify TextField.Msg

update : Msg -> Model -> (Model, Maybe Dispatch)
update msg model =
  case msg of
    RemoveSelf ->
      (model, Just Remove)

    Modify msg ->
      ({ model | field = TextField.update msg model.field }, Nothing)


-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ TextField.view model.field |> Html.App.map Modify
    , button [ onClick RemoveSelf ] [ text "X" ]
    ]
