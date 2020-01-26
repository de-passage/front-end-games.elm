module Minesweeper exposing (Model, Msg, init, update, view)

import Array exposing (Array)
import Css exposing (..)
import Dict exposing (Dict)
import Function as F
import Html.Events.Extra
import Html.Styled exposing (Attribute, Html, button, div, fieldset, input, text)
import Html.Styled.Attributes exposing (css, type_, value)
import Html.Styled.Events exposing (onClick)
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
    | Flagged
    | Marked
    | Revealed


type BoardT
    = BoardT


type alias Board =
    WrappedA Cell BoardT


type GameStatus
    = Ongoing
    | Won
    | Lost Int Int


type alias Model =
    { cells : Board
    , boardWidth : BoardWidth
    , boardHeight : BoardHeight
    , status : GameStatus
    , mineCount : MineCount
    }


type Msg
    = PlayedAt Int Int
    | Restart Int Int Int
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

type XT = XT 
type YT = YT
type alias X = WrappedI XT
type alias Y = WrappedI YT


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

countAdj : Model -> X -> Y -> Int
countAdj model x y = 
    let
        xp1 = W.succ x
        xm1 = W.pred x

        yp1 = W.succ y
        ym1 = W.pred y

        check = [(xm1, ym1), (xm1, y), (xm1, yp1), (x, ym1), (x, yp1), (xp1, ym1), (xp1, y), (xp1, yp1)]

        atCell v w = W.extract model.boardHeight * W.extract v + W.extract w

        cellValue (v, w) = case W.lift (Array.get (atCell v w)) model.cells of
                                Nothing -> Empty
                                Just (Cell content _) -> content
    in
        List.foldl (\v a -> if cellValue v == Mine then a + 1 else a) 0 check
    

viewCell : Model -> X -> Y -> Cell -> Html Msg
viewCell model x y (Cell cont visi) =
    let
        c =
            case cont of
                Empty ->
                    rgb 230 230 240

                Mine ->
                    rgb 138 3 3

        content =
            case cont of
                Empty ->
                    [text <| String.fromInt <| countAdj model x y]
                
                Mine ->
                    []

    in
    div [ css (cellStyle ++ [ backgroundColor c ]) ] content


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

        w = model.boardWidth

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
            [ css [ border2 (px 1) solid ]
            ]


view : Model -> Html Msg
view model =
    div []
        [ fieldset []
            [ heightInput model
            , widthInput model
            , mineInput model
            ]
        , button [ onClick RequestedNewList ] [ text "Generate" ]
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



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NewListGenerated array ->
            ( { model | cells = F.lift2 arrayOfMinePositionsToBoard .boardWidth .boardHeight model array }, Cmd.none )

        RequestedNewList ->
            ( model, F.lift3 generateNewList .boardWidth .boardHeight .mineCount model )

        HeightChanged m ->
            ( { model | boardHeight = m }, Cmd.none )

        WidthChanged m ->
            ( { model | boardWidth = m }, Cmd.none )

        MineCountChanged c ->
            ( { model | mineCount = c }, Cmd.none )

        _ ->
            ( model, Cmd.none )



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


black : Color
black =
    rgb 0 0 0


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
