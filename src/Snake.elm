module Snake exposing (Cell(..), Model, Msg, init, subscriptions, update, view)

import Array
import Browser.Dom exposing (Error(..), Viewport, getViewport)
import Browser.Events exposing (onResize)
import Css as Css exposing (..)
import CustomElements as CE
import Debug
import Html.Events.Extra exposing (targetValueIntParse)
import Html.Styled exposing (Attribute, Html, button, div, input, label, li, option, select, text, ul)
import Html.Styled.Attributes as Attributes exposing (css, max, min, selected, value)
import Html.Styled.Events exposing (on, onClick, onInput)
import Json.Decode as Decode
import Levels as Levels
import List.Extra as List
import Plane exposing (Coordinates, Height(..), Plane, Point, Row, Width(..), X(..), Y(..))
import Random
import Screens exposing (lgScreen, mdScreen, smScreen, xsScreen)
import Task
import Time


type Msg
    = Tick
    | None
    | DirectionChanged Direction
    | SpeedChanged Speed
    | NewTargetTick
    | TargetPositionGenerated ( X, Y )
    | Stop
    | Restart
    | Died
    | SetLevel Int
    | NameChanged String
    | GotNewSize Float Float
    | GotError String
    | Resized


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


type Level
    = Level (Plane Cell)
    | InvalidLevel String


type GameStatus
    = Running
    | Stopped
    | Dead


type alias Model =
    { board : Plane Cell
    , level : Int
    , snake : NonEmpty (Point Cell)
    , score : Score
    , direction : Direction
    , bufferedDirection : Direction
    , speed : Speed
    , tickSpeed : Float
    , targets : List (Point Cell)
    , log : String
    , status : GameStatus
    , highScores : List ( Score, String )
    , name : String
    , cellSize : Float
    }



-- CONSTANTS


zero : Score
zero =
    Score 0


maxSpeed : Speed
maxSpeed =
    Speed 5


minSpeed : Speed
minSpeed =
    Speed 1


height : Height
height =
    Height 20


width : Width
width =
    Width 50



-- INIT


emptyPlane : Plane Cell
emptyPlane =
    Plane.defaultInitialize height width Empty


makeSnake : Plane Cell -> NonEmpty (Point Cell)
makeSnake b =
    let
        s0 =
            Plane.clampToPoint (X 1) (Y 1) b
    in
    NonEmpty s0 (List.repeat 3 s0)


initialGame : Model
initialGame =
    let
        b =
            emptyPlane
    in
    { board = b
    , snake = makeSnake b
    , score = zero
    , direction = Right
    , bufferedDirection = Right
    , speed = minSpeed
    , tickSpeed = makeTickSpeed minSpeed
    , targets = []
    , level = 0
    , log = ""
    , status = Stopped
    , highScores = []
    , name = ""
    , cellSize = 10
    }


init : ( Model, Cmd Msg )
init =
    ( initialGame, getWindowSize )


getWindowSize : Cmd Msg
getWindowSize =
    let
        handleNewSize : Result Error Viewport -> Msg
        handleNewSize r =
            case r of
                Err _ ->
                    GotError "Couldn't find game plane"

                Ok viewport ->
                    GotNewSize viewport.viewport.width viewport.viewport.height
    in
    Task.attempt
        handleNewSize
        getViewport


makeTickSpeed : Speed -> Float
makeTickSpeed s =
    (fromSpeed maxSpeed - fromSpeed s + 1) * 100 |> toFloat



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.status of
        Running ->
            Sub.batch
                [ Time.every model.tickSpeed (always Tick)
                , Browser.Events.onKeyDown (decodeDirection model)
                , Time.every 1800 (always NewTargetTick)
                , onResize (\_ _ -> Resized)
                ]

        _ ->
            onResize (\_ _ -> Resized)


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
            [ a ]

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


isSnake : Coordinates -> NonEmpty (Point Cell) -> Bool
isSnake coord snake =
    liftNE2 List.any (sameCoordinates coord) snake


isTarget : Coordinates -> List (Point Cell) -> Bool
isTarget coord targets =
    List.any (sameCoordinates coord) targets


consNE : a -> NonEmpty a -> NonEmpty a
consNE a (NonEmpty b r) =
    NonEmpty a (b :: r)


fromScore : Score -> Int
fromScore (Score s) =
    s



-- VIEW


type CellType
    = EmptyCell
    | TargetCell
    | SnakeCell
    | WallCell


speedInput : Model -> Html Msg
speedInput model =
    CE.number (fromSpeed model.speed)
        [ Attributes.max (String.fromInt <| fromSpeed maxSpeed)
        , Attributes.min (String.fromInt <| fromSpeed minSpeed)
        , onInput stringToSpeed
        ]
        []


levelSelection : Model -> Html Msg
levelSelection model =
    Html.Styled.select
        [ on "change" (Decode.map SetLevel targetValueIntParse) ]
        (List.indexedMap (\i _ -> option [ value (String.fromInt i), selected (model.level == i) ] [ text ("level " ++ String.fromInt i) ]) levels)


cellType : Model -> Coordinates -> Cell -> CellType
cellType model coord cell =
    if isSnake coord model.snake then
        SnakeCell

    else if isTarget coord model.targets then
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
    div [ css (cellStyle model.cellSize ++ style) ] []


viewRow : Model -> Row Cell -> Html Msg
viewRow model array =
    div [ rowStyle ]
        (Array.toList <| Plane.mapIndexedRow (viewCell model) array)


viewOptions : Model -> Html Msg
viewOptions model =
    div
        [ optionDivStyle
        ]
        [ div [ speedInputStyle ]
            [ label [ speedInputLabelStyle ] [ text "Speed" ]
            , speedInput model
            ]
        , div [ levelSelectionStyle ]
            [ levelSelection model
            , text model.log
            ]
        ]


viewScores : Model -> Html Msg
viewScores model =
    let
        showScore ( s, n ) =
            li
                [ css
                    [ displayFlex
                    , justifyContent spaceBetween
                    ]
                ]
                [ div [ css [ maxWidth (pct 50) ] ] [ text n ]
                , div [ css [ maxWidth (pct 50) ] ] [ text (String.fromInt (fromScore s)) ]
                ]

        scores =
            case model.highScores of
                [] ->
                    [ text "No scores yet" ]

                l ->
                    List.map showScore l
    in
    ul
        [ css
            [ maxWidth (pct 100)
            , listStyle none
            , padding (px 0)
            ]
        ]
        scores


view : Model -> Html Msg
view model =
    let
        content =
            case model.status of
                Running ->
                    runningView

                Stopped ->
                    stoppedView

                Dead ->
                    deadView

        baseRunningView =
            [ div [] [ text "Score: ", text (String.fromInt (fromScore model.score)) ]
            , div
                [ css [ backgroundColor grey ]
                ]
                (Plane.mapRows (viewRow model) model.board)
            ]

        runningView =
            button [ onClick Stop ] [ text "Stop" ] :: baseRunningView

        deadView =
            div []
                [ input [ onInput NameChanged ] []
                , button [ onClick Died ] [ text "Game Over" ]
                ]
                :: (baseRunningView ++ [ viewScores model ])

        stoppedView =
            [ viewOptions model
            , button [ onClick Restart ] [ text "Start" ]
            , viewScores model
            ]
    in
    div
        [ mainDivStyle
        ]
        [ div
            [ contentStyle ]
            content
        ]



-- UPDATE


moveSnake : Model -> ( Model, Cmd Msg )
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

        nextCell =
            dirF (headNE model.snake)

        coord =
            Tuple.first <| Plane.fromPoint nextCell

        removeTarget c =
            List.filter (not << sameCoordinates c) model.targets
    in
    if Plane.at nextCell == Wall || isSnake coord model.snake then
        ( { model | status = Dead }, Cmd.none )

    else if isTarget coord model.targets then
        ( { model
            | targets = removeTarget coord
            , snake = consNE nextCell model.snake
            , score = Score (fromScore model.score + (501 - floor model.tickSpeed))
            , tickSpeed = model.tickSpeed * 0.95
          }
        , Cmd.none
        )

    else
        let
            newSnake =
                model.snake
                    |> initNE
                    |> NonEmpty nextCell
        in
        ( { model | snake = newSnake }, Cmd.none )


restart : Model -> Model
restart model =
    let
        s =
            makeSnake model.board
    in
    { model
        | snake = s
        , status = Running
        , targets = []
        , direction = Right
        , bufferedDirection = Right
        , tickSpeed = makeTickSpeed model.speed
        , score = zero
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            moveSnake { model | direction = model.bufferedDirection }

        DirectionChanged d ->
            ( { model | bufferedDirection = d }, Cmd.none )

        SpeedChanged s ->
            ( { model | speed = s, tickSpeed = makeTickSpeed s }, Cmd.none )

        NewTargetTick ->
            ( model, generateNewTarget model )

        TargetPositionGenerated ( x, y ) ->
            ( { model | targets = addTarget x y model }, Cmd.none )

        Restart ->
            ( restart model, Cmd.none )

        Stop ->
            ( { model | status = Stopped }, Cmd.none )

        Died ->
            let
                scores =
                    model.highScores
                        |> (\s -> ( model.score, model.name ) :: s)
                        |> List.sortBy (Tuple.first >> fromScore >> negate)
            in
            ( { model | status = Stopped, highScores = scores }, Cmd.none )

        None ->
            ( model, Cmd.none )

        SetLevel i ->
            case List.getAt i levels of
                Nothing ->
                    ( { model | log = "Invalid i received: " ++ String.fromInt i }, Cmd.none )

                Just level ->
                    validLevelFrom model i level

        NameChanged n ->
            ( { model | name = n }, Cmd.none )

        GotNewSize w _ ->
            ( { model | cellSize = w }, Cmd.none )

        GotError err ->
            ( { model | log = err }, Cmd.none )

        Resized ->
            ( model, getWindowSize )


validLevelFrom : Model -> Int -> Level -> ( Model, Cmd Msg )
validLevelFrom m i l =
    case l of
        InvalidLevel s ->
            ( { m | board = emptyPlane, log = s ++ "(level " ++ String.fromInt i ++ ")" }, Cmd.none )

        Level lv ->
            ( { m | board = lv, log = "" }, Cmd.none )


addTarget : X -> Y -> Model -> List (Point Cell)
addTarget x y model =
    let
        pos =
            Plane.wrapToPoint x y model.board

        coord =
            Tuple.first (Plane.fromPoint pos)
    in
    if Plane.at pos == Wall || isSnake coord model.snake || isTarget coord model.targets then
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


forXsScreen : (Float -> List Style) -> Style
forXsScreen f =
    xsScreen (f 100)


forSmScreen : (Float -> List Style) -> Style
forSmScreen f =
    smScreen (f 90)


forMdScreen : (Float -> List Style) -> Style
forMdScreen f =
    mdScreen (f 50)


forAllScreens : (Float -> List Style) -> List Style
forAllScreens f =
    List.map (\s -> s f) [ forXsScreen, forSmScreen, forMdScreen ]


mainDivStyle : Attribute msg
mainDivStyle =
    let
        computeDimensions p =
            [ Css.width (pct p), marginLeft (pct ((100 - p) / 2)) ]
    in
    css (displayFlex :: forAllScreens computeDimensions)


optionDivStyle : Attribute msg
optionDivStyle =
    css
        [ displayFlex
        , Css.flexDirection Css.column
        ]


speedInputStyle : Attribute msg
speedInputStyle =
    css []


speedInputLabelStyle : Attribute msg
speedInputLabelStyle =
    css [ Css.marginRight (em 1) ]


levelSelectionStyle : Attribute msg
levelSelectionStyle =
    css
        [ Css.marginTop (px 4)
        , Css.marginBottom (px 4)
        ]


contentStyle : Attribute msg
contentStyle =
    css
        [ margin auto
        , Css.minWidth (pct 50)
        ]


rowStyle : Attribute msg
rowStyle =
    css [ displayFlex ]


cellStyle : Float -> List Style
cellStyle cellSize =
    let
        computeSize p =
            [ Css.height (px ((cellSize / 50) * p / 100))
            , Css.width (px ((cellSize / 50) * p / 100))
            ]
    in
    [ overflow hidden
    , textOverflow ellipsis
    ]
        ++ forAllScreens computeSize



-- LEVELS


levels : List Level
levels =
    let
        cellFromEnum i =
            if i == 0 then
                Empty

            else
                Wall

        lvl0 =
            Array.initialize (20 * 50) (always 0)

        lvl1 =
            Array.fromList
                Levels.level1

        lvl2 =
            Array.fromList
                Levels.level2

        lvl3 =
            Array.fromList
                Levels.level3

        lvls =
            [ lvl0, lvl1, lvl2, lvl3 ]

        toLevel l =
            case
                l
                    |> Array.map cellFromEnum
                    |> Plane.fromArray height width Empty
            of
                Nothing ->
                    InvalidLevel "Level height/width don't match list representation."

                Just p ->
                    Level p
    in
    List.map toLevel lvls
