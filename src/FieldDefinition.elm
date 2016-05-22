module FieldDefinition exposing (..)

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import TextField


-- MODEL
type FieldType = Text | Number

type alias Model =
  { label: TextField.Model
  , id: TextField.Model
  , type': FieldType
  }

init : FieldType -> Int  -> Model
init type' index =
  { label =
    { label = "Label: "
    , id = "fieldlabel" ++ toString index
    , value = ""
    }
  , id =
    { label = "Id: "
    , id = "fieldid0" ++ toString index
    , value = ""
    }
  , type' = type'
  }


-- UPDATE
type Dispatch = Remove


type Msg
  = Modify String TextField.Msg
  | RemoveSelf


update : Msg -> Model -> (Model, Maybe Dispatch)
update msg model =
  case msg of
    Modify key msg ->
      case key of
        "label" ->
          ({ model | label = TextField.update msg model.label }, Nothing)

        "id" ->
          ({ model | id = TextField.update msg model.id }, Nothing)

        _ ->
          (model, Nothing)

    RemoveSelf ->
      (model, Just Remove)

-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ Html.map (Modify "label") (TextField.view model.label)
    , Html.map (Modify "id") (TextField.view model.id)
    , button [ onClick RemoveSelf ] [ text "X" ]
    ]
