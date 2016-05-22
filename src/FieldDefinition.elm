module FieldDefinition exposing (..)

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import TextField


-- MODEL
type alias Model =
  { label: TextField.Model
  , id: TextField.Model
  }

init : Model
init =
  { label = TextField.init 0
  , id = TextField.init 1
  }


-- UPDATE
type Msg
  = Modify String TextField.Msg


update : Msg -> Model -> Model
update msg model =
  case msg of
    Modify key msg ->
      case key of
        "label" ->
          { model | label = TextField.update msg model.label }

        "id" ->
          { model | label = TextField.update msg model.label }

        _ ->
          model


-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ Html.map (Modify "label") (TextField.view model.label)
    , Html.map (Modify "id") (TextField.view model.id)
    ]
