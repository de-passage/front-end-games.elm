module Snake exposing (..)

import Html.Styled exposing (..)
import Html.Styled.Events exposing (..)
import Css exposing (..)
import Plane exposing (..)
import Function exposing (..)

type alias Msg = ()
type alias Model = ()

init : (Model, Cmd Msg)
init = ((), Cmd.none)

view : Model -> Html Msg
view _ = text "Snake"

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)