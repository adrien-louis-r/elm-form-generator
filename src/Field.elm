module Field exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- MODEL
type FieldType
  = Text
  | Number
  | Select
  | Radio
  | Checkbox

type alias Model =
  { type': FieldType
  , label: String
  , id: String
  , value: String
  }

init : Int -> Model
init index =
  { type' = Text
  , label = "Text" ++ toString index
  , id = "text" ++ toString index
  , value = ""
  }


-- UPDATE
type Dispatch = Remove

type Msg
  = UpdateValue String
  | RemoveSelf

update : Msg -> Model -> (Model, Maybe Dispatch)
update msg model =
  case msg of
    UpdateValue newValue ->
      ( { model | value = newValue }, Nothing)
    RemoveSelf ->
      (model, Just Remove)


-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ label [ for model.id ] [ text model.label ]
    , input [ id model.id, onInput UpdateValue, value model.value ] []
    , button [ onClick RemoveSelf ] [ text "X" ]
    ]
