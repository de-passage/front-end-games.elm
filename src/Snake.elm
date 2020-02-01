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
import Random
import Time


type Msg
    = Tick
    | None
    | DirectionChanged Direction
    | SpeedChanged Speed
    | NewTargetTick
    | TargetPositionGenerated ( X, Y )
    | Restart


type Cell
    = Wall
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
    , targets : List (Point Cell)
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
    , targets = []
    }


init : ( Model, Cmd Msg )
init =
    ( initialGame, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (toFloat <| fromSpeed model.speed) (always Tick)
        , Browser.Events.onKeyDown (decodeDirection model)
        , Time.every 1800 (always NewTargetTick)
        ]


decodeDirection : Model -> Decode.Decoder Msg
decodeDirection model =
    let
        toDirection string =
            case string of
                "ArrowLeft" ->
                    if model.direction == Up || model.direction == Down then
                        DirectionChanged Left

                    else
                        None

                "ArrowRight" ->
                    if model.direction == Up || model.direction == Down then
                        DirectionChanged Right

                    else
                        None

                "ArrowUp" ->
                    if model.direction == Left || model.direction == Right then
                        DirectionChanged Up

                    else
                        None

                "ArrowDown" ->
                    if model.direction == Left || model.direction == Right then
                        DirectionChanged Down

                    else
                        None

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

    else if List.any (sameCoordinates coord) model.targets then
        TargetCell

    else
        case cell of
            Empty ->
                EmptyCell

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
        (speedInput model
            :: Plane.mapRows (viewRow model) model.board
        )



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

        NewTargetTick ->
            ( model, generateNewTarget model )

        TargetPositionGenerated ( x, y ) ->
            ( { model | targets = addTarget x y model }, Cmd.none )

        Restart -> 
            init

        None ->
            ( model, Cmd.none )


addTarget : X -> Y -> Model -> List (Point Cell)
addTarget x y model =
    let
        pos =
            Plane.wrapToPoint x y model.board

        coord =
            Tuple.first (Plane.fromPoint pos)
    in
    if Plane.at pos == Wall || liftNE2 List.any (sameCoordinates coord) model.snake || List.any (sameCoordinates coord) model.targets then
        model.targets

    else
        pos :: model.targets


generateNewTarget : Model -> Cmd Msg
generateNewTarget model =
    if List.length model.targets > 2 then
        Cmd.none

    else
        generateXY (Plane.width model.board) (Plane.height model.board)


generateXY : Width -> Height -> Cmd Msg
generateXY (Width w) (Height h) =
    Random.int 0 w
        |> Random.andThen (\y -> Random.int 0 h |> Random.map (\x -> ( X x, Y y )))
        |> Random.generate TargetPositionGenerated



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
