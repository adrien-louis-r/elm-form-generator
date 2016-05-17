module Field exposing (..)

import Html.App
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import TextField
import NumberField

-- MODEL
type Model
  = Text TextField.Model
  | Number NumberField.Model

initText : Int -> Model
initText index = Text (TextField.init index)

initNumber : Int -> Model
initNumber index = Number (NumberField.init index)

-- UPDATE
type Dispatch = Remove

type Msg
  = RemoveSelf
  | ModifyText TextField.Msg
  | ModifyNumber NumberField.Msg

update : Msg -> Model -> (Model, Maybe Dispatch)
update msg model =
  case (msg, model) of
    (RemoveSelf, _) ->
      (model, Just Remove)

    (ModifyText msg, Text fieldModel) ->
      (Text (TextField.update msg fieldModel), Nothing)

    (ModifyNumber msg, Number fieldModel) ->
      (Number (NumberField.update msg fieldModel), Nothing)

    _ ->
      (model, Nothing)

-- VIEW
view : Model -> Html Msg
view model =
  case model of
    Text fieldModel ->
      div []
        [ TextField.view fieldModel |> Html.App.map ModifyText
        , button [ onClick RemoveSelf ] [ text "X" ]
        ]

    Number fieldModel ->
      div []
        [ NumberField.view fieldModel |> Html.App.map ModifyNumber
        , button [ onClick RemoveSelf ] [ text "X" ]
        ]
