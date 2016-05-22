module NumberField exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String


-- MODEL
type alias Model =
  { label: String
  , id: String
  , value: Int
  }

init : String -> String -> Model
init label id =
  { label = label
  , id = id
  , value = 0
  }


-- UPDATE
type Msg
  = UpdateValue String

update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateValue newValue ->
      case String.toInt newValue of
        Ok int ->
          { model | value = int }
        Err _ ->
          { model | value = 0 }


-- VIEW
view : Model -> Html Msg
view model =
  span []
    [ label [ for model.id ] [ text model.label ]
    , input [ type' "number", id model.id, onInput UpdateValue, value "0" ] []
    ]
