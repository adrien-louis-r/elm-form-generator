module TextField exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- MODEL
type alias Model =
  { label: String
  , id: String
  , value: String
  }

init : String -> String -> Model
init label id =
  { label = label
  , id = id
  , value = ""
  }


-- UPDATE
type Msg
  = UpdateValue String

update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateValue newValue ->
      { model | value = newValue }


-- VIEW
view : Model -> Html Msg
view model =
  span []
    [ label [ for model.id ] [ text model.label ]
    , input [ type' "text", id model.id, onInput UpdateValue, value model.value ] []
    ]
