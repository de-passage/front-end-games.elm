module Minesweeper exposing (Model, Msg, init, update, view)

import Array exposing (Array)
import Css exposing (..)
import Dict exposing (Dict)
import Function as F
import Html.Events.Extra
import Html.Styled exposing (Attribute, Html, button, div, fieldset, input, text)
import Html.Styled.Attributes exposing (css, type_, value)
import Html.Styled.Events exposing (onClick, preventDefaultOn)
import Json.Decode as Json
import Random
import Wrapped as W exposing (..)


onChange =
    Html.Styled.Attributes.fromUnstyled << Html.Events.Extra.onChange


type Cell
    = Cell CellContent CellVisibility


type CellContent
    = Empty
    | Mine


type CellVisibility
    = Hidden
    | MineFlagged
    | Marked
    | Revealed


type BoardT
    = BoardT


type alias Board =
    WrappedA Cell BoardT


type GameStatus
    = Ongoing
    | Won
    | Lost
    | Stopped


type alias Model =
    { cells : Board
    , boardWidth : BoardWidth
    , boardHeight : BoardHeight
    , status : GameStatus
    , mineCount : MineCount
    }


type Msg
    = PlayedAt X Y
    | Flagged X Y
    | Restart
    | RequestedNewList
    | NewListGenerated (Array Int)
    | HeightChanged BoardHeight
    | WidthChanged BoardWidth
    | MineCountChanged MineCount


type BoardWidthT
    = BoardWidthT


type BoardHeightT
    = BoardHeightT


type MineCountT
    = MineCountT


type XT
    = XT


type YT
    = YT


type alias X =
    WrappedI XT


type alias Y =
    WrappedI YT


type alias BoardWidth =
    WrappedI BoardWidthT


type alias BoardHeight =
    WrappedI BoardHeightT


type alias MineCount =
    WrappedI MineCountT



-- VIEW
-- Inputs


number : Int -> List (Attribute msg) -> List (Html msg) -> Html msg
number val attr content =
    input (type_ "number" :: (value <| String.fromInt val) :: attr) content


msgWithDefault : (WrappedI a -> Msg) -> WrappedI a -> String -> Msg
msgWithDefault toMsg default received =
    case String.toInt received of
        Nothing ->
            toMsg default

        Just i ->
            toMsg (W.wrap i)


{-| Builds a Html input field of 'number' type, mapping to a wrapped integer.

    type MyInt = WrappedI MyTag
    type alias Model = { myInt : MyInt }
    type Msg = IntChanged MyInt

    inputForMyInt = customInput .myInt IntChanged

-}
customInput : (Model -> WrappedI a) -> (WrappedI a -> Msg) -> Model -> Html Msg
customInput get toMsg model =
    let
        v =
            get model
    in
    number (W.extract v) [ onChange (msgWithDefault toMsg v) ] []


heightInput : Model -> Html Msg
heightInput =
    customInput .boardHeight HeightChanged


widthInput : Model -> Html Msg
widthInput =
    customInput .boardWidth WidthChanged


mineInput : Model -> Html Msg
mineInput =
    customInput .mineCount MineCountChanged



-- Board


countAdj : X -> Y -> BoardWidth -> BoardHeight -> Board -> Int
countAdj x y w h board =
    let
        xp1 =
            W.succ x

        xm1 =
            W.pred x

        yp1 =
            W.succ y

        ym1 =
            W.pred y

        check =
            [ ( xm1, ym1 ), ( xm1, y ), ( xm1, yp1 ), ( x, ym1 ), ( x, yp1 ), ( xp1, ym1 ), ( xp1, y ), ( xp1, yp1 ) ]

        cellValue ( a, b ) =
            case W.lift (Array.get (atCell a b w h)) board of
                Nothing ->
                    Empty

                Just (Cell content _) ->
                    content
    in
    List.foldl
        (\v a ->
            if cellValue v == Mine then
                a + 1

            else
                a
        )
        0
        check


onRightClick : msg -> Attribute msg
onRightClick msg =
    preventDefaultOn "contextmenu" (Json.map alwaysPreventDefault (Json.succeed msg))


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


viewCell : Model -> X -> Y -> Cell -> Html Msg
viewCell model x y (Cell cont visi) =
    let
        mistake = visi == Marked && model.status == Lost && cont == Empty

        bgColor =
            case visi of
                Hidden ->
                    if cont == Mine && model.status == Lost then 
                        mineColor
                    else
                        hiddenCellColor

                MineFlagged ->
                    flaggedCellColor

                Marked ->
                    markedColor

                Revealed ->
                    if cont == Mine then
                        mineExplodedColor

                    else
                        emptyCellColor

        (content, textColor) =
            if cont == Empty && visi == Revealed then
                let
                    adj = F.lift3 (countAdj x y) .boardWidth .boardHeight .cells model
                in
                if adj /= 0  then
                    ([ text (String.fromInt adj) ], adjColor adj)
                else 
                    ([], white)

            else if mistake then
                ([ text "X" ], white)

            else
                ([], white)

        actions =
            if model.status == Ongoing then
                let
                    style =
                        if visi == Revealed then
                            [ cursor default ]

                        else
                            [ cursor pointer ]
                in
                [ onClick (PlayedAt x y), onRightClick (Flagged x y), css style ]

            else
                []

        attributes =
            css (cellStyle ++ [ backgroundColor bgColor, color textColor, fontWeight bold ]) :: actions
    in
    div attributes content


viewRow : Model -> X -> List Cell -> Html Msg
viewRow model x cells =
    cells
        |> List.indexedMap (W.wrapLift (viewCell model x))
        |> div [ css [ displayFlex ] ]


viewBoard : Model -> Html Msg
viewBoard model =
    let
        cellList =
            W.lift Array.toList model.cells

        w =
            model.boardWidth

        loop cells a =
            case cells of
                [] ->
                    a

                c ->
                    loop (W.lift List.drop w c) (a ++ [ W.lift List.take w c ])
    in
    loop cellList []
        |> List.indexedMap (W.wrapLift (viewRow model))
        |> div
            [ css [ border2 (px 1) solid, margin auto, maxWidth fitContent ]
            ]


statusString : GameStatus -> String
statusString s =
    case s of
        Won ->
            "Victory!"

        Lost ->
            "Kaboom!"

        _ ->
            ""


view : Model -> Html Msg
view model =
    div
        [ css
            [ margin auto
            , maxWidth fitContent
            ]
        ]
        [ fieldset []
            [ heightInput model
            , widthInput model
            , mineInput model
            ]
        , button [ onClick RequestedNewList ] [ text "Restart" ]
        , text (statusString model.status)
        , viewBoard model
        ]



-- UTILITIES


arrayOfMinePositionsToBoard : BoardWidth -> BoardHeight -> Array Int -> Board
arrayOfMinePositionsToBoard width height array =
    let
        result =
            Array.initialize (W.lift2 (*) width height) (always (Cell Empty Hidden))

        go rs list =
            case list of
                [] ->
                    rs

                i :: is ->
                    go (Array.set i (Cell Mine Hidden) rs) is
    in
    W.wrap (go result (Array.toList array))


generateNewList : BoardWidth -> BoardHeight -> MineCount -> Cmd Msg
generateNewList width height mineCount =
    let
        length =
            extract height * extract width

        generator =
            W.lift reservoirSample mineCount (Array.initialize length identity)
    in
    Random.generate NewListGenerated generator



-- Its a hack!


atCell : X -> Y -> BoardWidth -> BoardHeight -> Int
atCell x y w h =
    let
        xp =
            extract x

        yp =
            extract y

        wp =
            extract w

        hp =
            extract h
    in
    if xp >= hp || xp < 0 || yp < 0 || yp >= wp then
        -1

    else
        xp * wp + yp



-- UPDATE


allRevealed : Board -> Bool
allRevealed b =
    b
        |> W.lift Array.toList
        |> List.all (\(Cell c v) -> c == Mine || v == Revealed)


getStatus : CellContent -> Board -> GameStatus
getStatus cell cells =
    if cell == Mine then
        Lost

    else if allRevealed cells then
        Won

    else
        Ongoing


floodFill : X -> Y -> BoardWidth -> BoardHeight -> Board -> Board
floodFill x y w h cells =
    let
        pos =
            atCell x y w h

        ( content, visibility ) =
            case W.lift (Array.get pos) cells of
                Nothing ->
                    ( Empty, Revealed )

                Just (Cell c v) ->
                    ( c, v )

        revealed =
            visibility == Revealed

        mine =
            content == Mine

        reveal =
            W.liftW (Array.set pos (Cell content Revealed))

        revealIfNoAdjacent xp yp wp hp b =
            if countAdj xp yp wp hp b == 0 then
                floodFill xp yp wp hp b

            else
                b
    in
    if revealed || mine then
        cells

    else if countAdj x y w h cells == 0 then
        reveal cells
            |> floodFill (W.succ x) y w h
            |> floodFill (W.pred x) y w h
            |> floodFill x (W.succ y) w h
            |> floodFill x (W.pred y) w h
            |> floodFill (W.pred x) (W.pred y) w h
            |> floodFill (W.succ x) (W.pred y) w h
            |> floodFill (W.pred x) (W.succ y) w h
            |> floodFill (W.succ x) (W.succ y) w h

    else
        reveal cells
            |> revealIfNoAdjacent (W.succ x) y w h
            |> revealIfNoAdjacent (W.pred x) y w h
            |> revealIfNoAdjacent x (W.succ y) w h
            |> revealIfNoAdjacent x (W.pred y) w h


playAt : Model -> X -> Y -> Model
playAt model x y =
    let
        pos =
            atCell x y model.boardWidth model.boardHeight
    in
    case Array.get pos (W.extract model.cells) of
        Nothing ->
            model

        Just (Cell c _) ->
            let
                nCells =
                    model.cells
                        |> floodFill x y model.boardWidth model.boardHeight
                        |> W.liftW (Array.set pos (Cell c Revealed))

                status =
                    getStatus c nCells
            in
            { model | cells = nCells, status = status }


nextFlag : CellVisibility -> CellVisibility
nextFlag f =
    case f of
        Hidden ->
            Marked

        Marked ->
            MineFlagged

        MineFlagged ->
            Hidden

        Revealed ->
            Revealed


toggleCell : Model -> X -> Y -> Model
toggleCell model x y =
    let
        pos =
            atCell x y model.boardWidth model.boardHeight
    in
    case Array.get pos (W.extract model.cells) of
        Nothing ->
            model

        Just (Cell c v) ->
            { model | cells = W.liftW (Array.set pos (Cell c (nextFlag v))) model.cells }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NewListGenerated array ->
            ( { model | cells = F.lift2 arrayOfMinePositionsToBoard .boardWidth .boardHeight model array, status = Ongoing }, Cmd.none )

        RequestedNewList ->
            ( model, F.lift3 generateNewList .boardWidth .boardHeight .mineCount model )

        HeightChanged m ->
            ( { model | boardHeight = m }, Cmd.none )

        WidthChanged m ->
            ( { model | boardWidth = m }, Cmd.none )

        MineCountChanged c ->
            ( { model | mineCount = c }, Cmd.none )

        Flagged x y ->
            ( toggleCell model x y, Cmd.none )

        PlayedAt x y ->
            ( playAt model x y, Cmd.none )

        Restart ->
            ( model, F.lift3 generateNewList .boardWidth .boardHeight .mineCount model )



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { cells = emptyBoard
      , boardWidth = W.wrap 10
      , boardHeight = W.wrap 10
      , status = Ongoing
      , mineCount = W.wrap 10
      }
    , F.lift3 generateNewList W.wrap W.wrap W.wrap 10
    )


emptyBoard : Board
emptyBoard =
    W.wrap Array.empty



-- CSS STYLES


mineColor : Color
mineColor =
    rgb 200 0 0


emptyCellColor : Color
emptyCellColor =
    rgb 230 230 240


hiddenCellColor : Color
hiddenCellColor =
    rgb 110 110 110


markedColor : Color
markedColor =
    rgb 75 0 0


flaggedCellColor : Color
flaggedCellColor =
    rgb 204 102 255


mineExplodedColor : Color
mineExplodedColor =
    rgb 255 160 0


black : Color
black =
    rgb 0 0 0

white : Color
white =
    rgb 255 255 255


adjColor : Int -> Color
adjColor i = case i of 
    1 -> blue
    2 -> red
    3 -> green
    4 -> purple
    5 -> orange
    6 -> cyan
    7 -> magenta
    8 -> yellow
    _ -> white

blue : Color
blue = rgb 0 0 255

red : Color
red = rgb 255 0 0

green : Color
green = rgb 0 0 255

yellow : Color
yellow = rgb 255 255 0

magenta : Color
magenta = rgb 255 0 255

cyan : Color 
cyan = rgb 0 255 255

orange : Color
orange = rgb 255 165 0

purple : Color
purple = rgb 128 0 128

cellSize : Em
cellSize =
    em 1


cellStyle : List Style
cellStyle =
    [ border3 (px 1) solid black
    , overflow hidden
    , textOverflow ellipsis
    , width cellSize
    , height cellSize
    , textAlign center
    ]



-- RANDOM GENERATOR


{-| Adapted from wikipedia <https://en.wikipedia.org/wiki/Reservoir_sampling>
-}
reservoirSample : Int -> Array a -> Random.Generator (Array a)
reservoirSample n source =
    let
        max =
            Array.length source

        randomF : Random.Generator Float
        randomF =
            Random.float 0 1

        result =
            Array.slice 0 n source

        w : Random.Generator Float
        w =
            randomF
                |> Random.map (\r -> e ^ (logBase e r / toFloat (n - 1)))

        next : Int -> Float -> Random.Generator Int
        next i x =
            randomF
                |> Random.map (\r -> 1 + i + floor (logBase e r / logBase e (1 - x)))

        repl i list k =
            case Array.get i source of
                Nothing ->
                    list

                Just x ->
                    Array.set k x list

        loop : Int -> Array a -> Float -> Random.Generator (Array a)
        loop i list x =
            if i >= max then
                Random.constant list

            else
                Random.int 0 (n - 1)
                    |> Random.andThen
                        (\k ->
                            next i x
                                |> Random.andThen
                                    (\j ->
                                        w
                                            |> Random.andThen (\y -> loop j (repl i list k) (x * y))
                                    )
                        )
    in
    if n <= 0 || max == 0 then
        Random.constant Array.empty

    else if n == 1 then
        Random.int 0 (max - 1)
            |> Random.map
                (\i ->
                    case Array.get i source of
                        Just x ->
                            Array.fromList [ x ]

                        Nothing ->
                            Array.empty
                )

    else
        w
            |> Random.andThen
                (\x ->
                    next (n - 1) x
                        |> Random.andThen (\j -> loop j result x)
                )
