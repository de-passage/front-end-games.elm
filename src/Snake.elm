module Snake exposing (Cell(..), Model, Msg, init, subscriptions, update, view)

import Array exposing (Array)
import Browser.Events
import Css exposing (..)
import Function exposing (..)
import Html.Styled exposing (Attribute, Html, div, input, text)
import Html.Styled.Attributes exposing (css, max, min, type_, value)
import Html.Styled.Events exposing (..)
import Json.Decode as Decode
import List.Extra as List
import Plane exposing (Coordinates, Height(..), Plane, Point, Row, Width(..), X(..), Y(..))
import Time


type Msg
    = Tick
    | None
    | DirectionChanged Direction
    | SpeedChanged Speed


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


type Speed
    = Speed Int


type NonEmpty a
    = NonEmpty a (List a)


type alias Model =
    { board : Plane Cell
    , snake : NonEmpty (Point Cell)
    , score : Score
    , direction : Direction
    , speed : Speed
    }



-- CONSTANTS


zero : Score
zero =
    Score 0


maxSpeed : Speed
maxSpeed =
    Speed 1000


minSpeed : Speed
minSpeed =
    Speed 100



-- INIT


initialGame : Model
initialGame =
    let
        b =
            Plane.defaultInitialize (Height 20) (Width 50) Empty

        s0 =
            Plane.clampToPoint (X 0) (Y 0) b
    in
    { board = b
    , snake = NonEmpty s0 (List.repeat 3 s0)
    , score = zero
    , direction = Right
    , speed = Speed 500
    }


init : ( Model, Cmd Msg )
init =
    ( initialGame, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Time.every (toFloat <| fromSpeed model.speed) (always Tick), Browser.Events.onKeyDown (decodeDirection model) ]


decodeDirection : Model -> Decode.Decoder Msg
decodeDirection model =
    let
        toDirection string =
            case string of
                "ArrowLeft" ->
                    DirectionChanged Left

                "ArrowRight" ->
                    DirectionChanged Right

                "ArrowUp" ->
                    DirectionChanged Up

                "ArrowDown" ->
                    DirectionChanged Down

                _ ->
                    None
    in
    Decode.map toDirection (Decode.field "key" Decode.string)



-- UTILITY


sameCoordinates : Coordinates -> Point a -> Bool
sameCoordinates c p =
    Plane.coordinatesEqual (Tuple.first (Plane.fromPoint p)) c


liftNE : (List a -> b) -> NonEmpty a -> b
liftNE f (NonEmpty a r) =
    f (a :: r)


liftNE2 : (c -> List a -> b) -> c -> NonEmpty a -> b
liftNE2 f c (NonEmpty a r) =
    f c (a :: r)


headNE : NonEmpty a -> a
headNE (NonEmpty a _) =
    a


initNE : NonEmpty a -> List a
initNE (NonEmpty a r) =
    case List.init r of
        Nothing ->
            []

        Just l ->
            a :: l


fromSpeed : Speed -> Int
fromSpeed (Speed s) =
    s


stringToSpeed : String -> Msg
stringToSpeed string =
    case String.toInt string of
        Nothing ->
            SpeedChanged maxSpeed

        Just i ->
            SpeedChanged (Speed i)



-- VIEW


type CellType
    = EmptyCell
    | TargetCell
    | SnakeCell
    | WallCell


number : Int -> List (Attribute msg) -> List (Html msg) -> Html msg
number val attr content =
    input (type_ "number" :: (value <| String.fromInt val) :: attr) content


speedInput : Model -> Html Msg
speedInput model =
    number (fromSpeed model.speed) [ max (String.fromInt <| fromSpeed maxSpeed), min (String.fromInt <| fromSpeed minSpeed), onInput stringToSpeed ] []


cellType : Model -> Coordinates -> Cell -> CellType
cellType model coord cell =
    if liftNE2 List.any (sameCoordinates coord) model.snake then
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
        (speedInput model ::
        (Plane.mapRows (viewRow model) model.board) )



-- UPDATE


moveSnake : Model -> Model
moveSnake model =
    let
        dirF =
            case model.direction of
                Left ->
                    Plane.wrapPointLeft

                Right ->
                    Plane.wrapPointRight

                Up ->
                    Plane.wrapPointUp

                Down ->
                    Plane.wrapPointDown

        newSnake =
            model.snake
                |> initNE
                |> NonEmpty (dirF (headNE model.snake))
    in
    { model | snake = newSnake }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( moveSnake model, Cmd.none )

        DirectionChanged d ->
            ( { model | direction = d }, Cmd.none )

        SpeedChanged s ->
            ( { model | speed = s }, Cmd.none )

        None ->
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
