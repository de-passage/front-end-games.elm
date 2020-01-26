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


type Board
    = Board (Array Cell)


type GameStatus
    = Ongoing
    | Won
    | Lost ( Int, Int )


type alias Model =
    { cells : Board
    , boardWidth : BoardWidth
    , boardHeight : BoardHeight
    , status : GameStatus
    , mineCount : MineCount
    }


type Msg
    = PlayedAt ( Int, Int )
    | Restart ( Int, Int, Int )
    | RequestedNewList
    | NewListGenerated (Array Int)
    | HeightChanged BoardHeight
    | WidthChanged BoardWidth
    | MineCountChanged MineCount


type BoardWidth
    = BoardWidth Int


type BoardHeight
    = BoardHeight Int


type MineCount
    = MineCount Int



-- VIEW


number : Int -> List (Attribute msg) -> List (Html msg) -> Html msg
number val attr content =
    input (type_ "number" :: (value <| String.fromInt val) :: attr) content


viewCell : Cell -> Html Msg
viewCell (Cell cont _) =
    let
        c =
            case cont of
                Empty ->
                    rgb 230 230 240

                Mine ->
                    rgb 138 3 3
    in
    div [ css (cellStyle ++ [ backgroundColor c ]) ] []


viewRow : List Cell -> Html Msg
viewRow cells =
    cells
        |> List.map viewCell
        |> div [ css [ displayFlex ] ]


viewBoard : Board -> BoardWidth -> Html Msg
viewBoard (Board array) (BoardWidth width) =
    let
        cellList =
            Array.toList array

        loop cells a =
            case cells of
                [] ->
                    a

                c ->
                    loop (List.drop width c) (a ++ [ List.take width c ])
    in
    loop cellList []
        |> List.map viewRow
        |> div [ css [ border2 (px 1) solid ] ]


msgWithDefault : (Int -> a) -> (a -> Msg) -> a -> String -> Msg
msgWithDefault fromInt toMsg default received =
    case String.toInt received of
        Nothing ->
            toMsg default

        Just i ->
            toMsg (fromInt i)


customInput : (Model -> a) -> (a -> Int) -> (Int -> a) -> (a -> Msg) -> Model -> Html Msg
customInput get toInt fromInt toMsg model =
    number (toInt (get model)) [ onChange (msgWithDefault fromInt toMsg (get model)) ] []


heightInput : Model -> Html Msg
heightInput =
    customInput .boardHeight heightToInt BoardHeight HeightChanged


widthInput : Model -> Html Msg
widthInput =
    customInput .boardWidth widthToInt BoardWidth WidthChanged


mineInput : Model -> Html Msg
mineInput =
    customInput .mineCount mineCountToInt MineCount MineCountChanged


view : Model -> Html Msg
view model =
    div []
        [ fieldset [] [ heightInput model, widthInput model, mineInput model ]
        , button [ onClick RequestedNewList ] [ text "Generate" ]
        , F.lift2 viewBoard .cells .boardWidth model
        ]



-- UTILITIES

widthToInt : BoardWidth -> Int
widthToInt (BoardWidth i) =
    i


heightToInt : BoardHeight -> Int
heightToInt (BoardHeight i) =
    i


mineCountToInt : MineCount -> Int
mineCountToInt (MineCount i) =
    i



arrayOfMinePositionsToBoard : BoardWidth -> BoardHeight -> Array Int -> Board
arrayOfMinePositionsToBoard (BoardWidth width) (BoardHeight height) array =
    let
        result =
            Array.initialize (width * height) (always (Cell Empty Hidden))

        go rs list =
            case list of
                [] ->
                    rs

                i :: is ->
                    go (Array.set i (Cell Mine Hidden) rs) is
    in
    Board (go result (Array.toList array))


generateNewList : BoardWidth -> BoardHeight -> MineCount -> Cmd Msg
generateNewList (BoardWidth width) (BoardHeight height) (MineCount mineCount) =
    let
        length =
            height * width

        generator =
            reservoirSample mineCount (Array.initialize length identity)
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
      , boardWidth = BoardWidth 10
      , boardHeight = BoardHeight 10
      , status = Ongoing
      , mineCount = MineCount 10
      }
    , F.lift3 generateNewList BoardWidth BoardHeight MineCount 10
    )


emptyBoard : Board
emptyBoard =
    Board Array.empty



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
