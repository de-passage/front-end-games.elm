module Snake exposing (Cell(..), Model, Msg, init, update, view)

import Array exposing (Array)
import Css exposing (..)
import Function exposing (..)
import Html.Styled exposing (Attribute, Html, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (..)
import List.Extra as List
import Plane exposing (Coordinates, Height(..), Plane, Point, Row, Width(..), X(..), Y(..))


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



-- INIT


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



-- UTILITY


sameCoordinates : Coordinates -> Point a -> Bool
sameCoordinates c p =
    Plane.coordinatesEqual (Tuple.first (Plane.fromPoint p)) c



-- VIEW


type CellType
    = EmptyCell
    | TargetCell
    | SnakeCell
    | WallCell


cellType : Model -> Coordinates -> Cell -> CellType
cellType model coord cell =
    if List.any (sameCoordinates coord) model.snake then
        SnakeCell

    else
        case cell of
            Empty ->
                EmptyCell

            Target ->
                TargetCell

            Wall ->
                WallCell


viewCell : Model -> Coordinates -> Cell -> Html Msg
viewCell model coordinates cell =
    let
        kind =
            cellType model coordinates cell

        style =
            case kind of
                SnakeCell ->
                    [ backgroundColor green, borderRadius (pct 50) ]

                EmptyCell ->
                    []

                WallCell ->
                    [ backgroundColor black ]

                TargetCell ->
                    [ backgroundColor red ]

    in
    div [ css (cellStyle ++ style) ] []


viewRow : Model -> Row Cell -> Html Msg
viewRow model array =
    div [ css [ displayFlex ] ]
        (Array.toList <| Plane.mapIndexedRow (viewCell model) array)


view : Model -> Html Msg
view model =
    div
        [ css
            [ margin auto
            , maxWidth fitContent
            , backgroundColor grey
            ]
        ]
        (Plane.mapRows (viewRow model) model.board)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- CSS STYLES


cellSize : Em
cellSize =
    em 1


cellStyle : List Style
cellStyle =
    [ overflow hidden
    , textOverflow ellipsis
    , width cellSize
    , height cellSize
    ]


grey : Color
grey =
    rgb 175 175 175


green : Color
green =
    rgb 0 255 0


black : Color
black =
    rgb 0 0 0


red : Color
red =
    rgb 255 0 0
