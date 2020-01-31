module Snake exposing (Cell(..), Model, Msg, init, update, view)

import Array exposing (Array)
import Css exposing (..)
import Function exposing (..)
import Html.Styled exposing (Attribute, Html, text, div)
import Html.Styled.Events exposing (..)
import Html.Styled.Attributes exposing (css)
import Plane exposing (Height(..), Plane, Point, Width(..), X(..), Y(..), Row, Coordinates)


type alias Msg =
    ()


type Cell
    = Target
    | Wall
    | Empty


type Direction
    = Left
    | Right
    | Up
    | Down


type Score
    = Score Int


type alias Model =
    { board : Plane Cell
    , snake : List (Point Cell)
    , score : Score
    }


zero : Score
zero =
    Score 0


initialGame : Model
initialGame =
    let
        b =
            Plane.defaultInitialize (Height 20) (Width 50) Empty

        s0 =
            Plane.clampToPoint (X 0) (Y 0) b
    in
    { board = b
    , snake = List.repeat 3 s0
    , score = zero
    }


init : ( Model, Cmd Msg )
init =
    ( initialGame, Cmd.none )


viewCell : Coordinates -> Cell -> Html Msg
viewCell coordinates cell =
    div [ css cellStyle ] []

viewRow : Row Cell -> Html Msg
viewRow array =
    div [ css [displayFlex] ] 
        (Array.toList <| Plane.mapIndexedRow viewCell array)

view : Model -> Html Msg
view model =
    div
        [ css
            [ margin auto
            , maxWidth fitContent
            ]
        ]
        (Plane.mapRows viewRow model.board)
        


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


-- CSS STYLES

cellSize : Em
cellSize = em 1

cellStyle : List Style
cellStyle =
    [ overflow hidden
    , textOverflow ellipsis
    , width cellSize
    , height cellSize
    ]