import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Random


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL
type alias Model = String


init : (Model, Cmd Msg)
init =
  ("", Cmd.none)


-- UPDATE
type Msg
  = SayHi


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SayHi ->
      ("Hi!", Cmd.none)


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text model ]
    , button [ onClick SayHi ] [ text "Hi" ]
    ]
