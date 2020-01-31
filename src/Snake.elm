module Snake exposing (Cell(..), Model, Msg, init, update, view)

import Css exposing (..)
import Function exposing (..)
import Html.Styled exposing (Attribute, Html, text)
import Html.Styled.Events exposing (..)
import Plane exposing (Plane, Point)


type alias Msg =
    ()


type Cell
    = Target
    | Wall
    | Empty

type Direction = Left | Right | Up | Down

type alias Model =
    { board : Plane Cell
    , snake : List (Point Cell)
    , score : Int
    }


init : ( Model, Cmd Msg )
init =
    ( (), Cmd.none )


view : Model -> Html Msg
view _ =
    text "Snake"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
