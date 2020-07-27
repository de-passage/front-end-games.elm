module Tetris exposing (Model, Msg, init, update, view)

import Html.Styled exposing (Html, div)


type alias Model =
    {}


type alias Msg =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


view : Model -> Html Msg
view _ =
    div [] []


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )
