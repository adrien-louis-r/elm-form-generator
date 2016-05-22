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

initText : String -> String -> Model
initText label id = Text (TextField.init label id)

initNumber : String -> String -> Model
initNumber label id = Number (NumberField.init label id)

-- UPDATE
type Msg
  = ModifyText TextField.Msg
  | ModifyNumber NumberField.Msg

update : Msg -> Model -> Model
update msg model =
  case (msg, model) of
    (ModifyText msg, Text fieldModel) ->
      Text (TextField.update msg fieldModel)

    (ModifyNumber msg, Number fieldModel) ->
      Number (NumberField.update msg fieldModel)

    _ ->
      model

-- VIEW
view : Model -> Html Msg
view model =
  case model of
    Text fieldModel ->
      div []
        [ TextField.view fieldModel |> Html.App.map ModifyText ]

    Number fieldModel ->
      div []
        [ NumberField.view fieldModel |> Html.App.map ModifyNumber ]
