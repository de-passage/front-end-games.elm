module TicTacToe exposing (Controller(..), Model, Msg(..), init, update, view)

import Css exposing (..)
import Dict exposing (Dict)
import Html.Styled exposing (Attribute, Html, button, div, table, tbody, td, text, tr)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import List.Extra as List
import Maybe.Extra as Maybe
import Tuple


type Player
    = Player Int


player1 : Player
player1 =
    Player 1


player2 : Player
player2 =
    Player 2


type Cell
    = Played Player
    | EmptyCell


type Board
    = Board (Dict Int Cell)


type Model
    = GameOver GameOverModel
    | GameOngoing GameOngoingModel


type alias GameOngoingModel =
    { board : Board
    , currentPlayer : Player
    }


type alias GameOverModel =
    { board : Board
    , state : GameState
    }


type Controller
    = Human
    | Computer


type Msg
    = PlayAt Position
    | Restart
    | Surrender


type Position
    = Position Int


type GameState
    = Won Victory
    | Draw
    | OnGoing
    | Surrendered Player


type alias IndexedCell =
    ( Position, Cell )


type alias Victory =
    ( List Position, Player )


availablePositions : Board -> List Position
availablePositions (Board cells) =
    cells
        |> Dict.toList
        |> List.filter (Tuple.second >> isEmpty)
        |> List.map (Tuple.first >> Position)


play : Board -> Position -> Player -> Maybe Board
play (Board cells) (Position i) player =
    case Dict.get i cells of
        Just EmptyCell ->
            cells
                |> Dict.update i (Maybe.map <| always <| Played player)
                |> Board
                |> Just

        _ ->
            Nothing


samePlayer : Player -> Player -> Bool
samePlayer (Player a) (Player b) =
    a == b


samePosition : Position -> Position -> Bool
samePosition (Position a) (Position b) =
    a == b


cellPlayer : Cell -> Maybe Player
cellPlayer cell =
    case cell of
        EmptyCell ->
            Nothing

        Played player ->
            Just player


allSamePlayer : Dict Int Cell -> List Int -> Maybe Victory
allSamePlayer dict pos =
    let
        players =
            Maybe.combine <| List.map (\i -> Dict.get i dict |> Maybe.andThen cellPlayer) pos

        positions =
            List.map Position pos
    in
    players
        |> Maybe.andThen List.head
        |> Maybe.andThen
            (\p ->
                players
                    |> Maybe.andThen
                        (\l ->
                            if List.all (samePlayer p) l then
                                Just p

                            else
                                Nothing
                        )
            )
        |> Maybe.map (\p -> ( positions, p ))


winConditions : List (List Int)
winConditions =
    [ [ 0, 3, 6 ], [ 1, 4, 7 ], [ 2, 5, 8 ], [ 0, 1, 2 ], [ 3, 4, 5 ], [ 6, 7, 8 ], [ 0, 4, 8 ], [ 2, 4, 6 ] ]


nextPlayer : Player -> Player
nextPlayer player =
    if samePlayer player player1 then
        player2

    else
        player1


gameState : Board -> GameState
gameState (Board cells) =
    winConditions
        |> List.map (allSamePlayer cells)
        |> List.find Maybe.isJust
        |> Maybe.join
        |> (\mb ->
                case mb of
                    Nothing ->
                        if Dict.values cells |> List.any isEmpty then
                            OnGoing

                        else
                            Draw

                    Just player ->
                        Won player
           )


isEmpty : Cell -> Bool
isEmpty c =
    case c of
        EmptyCell ->
            True

        _ ->
            False


emptyBoard : Board
emptyBoard =
    List.repeat 9 EmptyCell |> List.indexedMap Tuple.pair |> Dict.fromList |> Board


boardAsTable : Board -> List (List IndexedCell)
boardAsTable (Board cells) =
    let
        c =
            Dict.toList cells |> List.map (\( i, cell ) -> ( Position i, cell ))
    in
    [ List.take 3 c
    , List.take 3 <| List.drop 3 c
    , List.drop 6 c
    ]


symbolFor : Player -> Html msg
symbolFor player =
    text <|
        if player == player1 then
            "X"

        else
            "O"


cellValue : IndexedCell -> Cell
cellValue =
    Tuple.second


cellPosition : IndexedCell -> Position
cellPosition =
    Tuple.first


type Clickable
    = Clickable
    | NonClickable


type alias CellDecorator =
    IndexedCell -> List (Attribute Msg)


isClickable : Clickable -> Bool
isClickable c =
    case c of
        Clickable ->
            True

        NonClickable ->
            False


viewCell : CellDecorator -> Clickable -> IndexedCell -> Html Msg
viewCell decorate click cell =
    let
        styledTd attributes =
            td ((cellStyle :: attributes) ++ decorate cell)
    in
    case cellValue cell of
        Played player ->
            styledTd [] [ symbolFor player ]

        EmptyCell ->
            styledTd
                (if isClickable click then
                    [ onClick (PlayAt <| cellPosition cell) ]

                 else
                    []
                )
                []


viewRow : CellDecorator -> Clickable -> List IndexedCell -> Html Msg
viewRow d f cells =
    tr [] <| List.map (viewCell d f) cells


viewBoard : CellDecorator -> Clickable -> List (List IndexedCell) -> Html Msg
viewBoard d f b =
    table [ tableStyle ] [ tbody [] (List.map (viewRow d f) b) ]


viewGame : Board -> List (Html Msg) -> CellDecorator -> Clickable -> Html Msg
viewGame board control decorate clickable =
    div
        [ css
            [ margin auto
            , maxWidth fitContent
            ]
        ]
        [ div [] control
        , viewBoard decorate clickable (boardAsTable board)
        ]


decorateVictory : GameState -> CellDecorator
decorateVictory state ( cellIndex, _ ) =
    case state of
        Won ( positions, _ ) ->
            if Maybe.isJust (List.find (samePosition cellIndex) positions) then
                winningCellStyle

            else
                []

        _ ->
            []


gameEndedControlBoard : GameOverModel -> List (Html Msg)
gameEndedControlBoard game =
    [ div []
        [ text (gameStateToString game.state) ]
    , div [ buttonDivStyle ]
        [ button [ onClick Restart ] [ text "Restart" ]
        ]
    ]


gameOnGoingControlBoard : GameOngoingModel -> List (Html Msg)
gameOnGoingControlBoard game =
    [ div []
        [ div [] [ text (playerTurnText game.currentPlayer) ]
        , div [ buttonDivStyle ]
            [ button [ onClick Surrender ] [ text "Surrender" ]
            ]
        ]
    ]


buttonDivStyle : Attribute Msg
buttonDivStyle =
    css
        [ margin (px 5)
        ]


view : Model -> Html Msg
view model =
    case model of
        GameOver m ->
            viewGame m.board (gameEndedControlBoard m) (decorateVictory m.state) NonClickable

        GameOngoing m ->
            viewGame m.board (gameOnGoingControlBoard m) (always []) Clickable


ongoing : Model -> (GameOngoingModel -> Model) -> Model
ongoing model f =
    case model of
        GameOver _ ->
            model

        GameOngoing m ->
            f m


update : Msg -> Model -> Model
update msg model =
    case msg of
        Restart ->
            init

        Surrender ->
            ongoing model <|
                \m ->
                    GameOver { board = m.board, state = Surrendered m.currentPlayer }

        PlayAt pos ->
            ongoing model <|
                \m ->
                    case play m.board pos m.currentPlayer of
                        Nothing ->
                            model

                        Just board ->
                            case gameState board of
                                OnGoing ->
                                    GameOngoing { board = board, currentPlayer = nextPlayer m.currentPlayer }

                                state ->
                                    GameOver { board = board, state = state }


init : Model
init =
    GameOngoing { board = emptyBoard, currentPlayer = player1 }


gameStateToString : GameState -> String
gameStateToString state =
    case state of
        Won ( _, Player i ) ->
            "Player " ++ String.fromInt i ++ " has won!"

        Draw ->
            "It's a draw!"

        OnGoing ->
            "The game is not over yet"

        Surrendered (Player i) ->
            "Player " ++ String.fromInt i ++ " surrendered..."


playerTurnText : Player -> String
playerTurnText (Player i) =
    "It is player " ++ String.fromInt i ++ "'s turn to play."


refSize : Float
refSize =
    15


boardSize : Em
boardSize =
    em refSize


cellSize : Em
cellSize =
    em (refSize / 3)


tableStyle : Attribute Msg
tableStyle =
    css
        [ tableLayout fixed
        , width boardSize
        , height boardSize
        , border2 (px 1) solid
        , borderSpacing (px 0)
        ]


cellStyle : Attribute Msg
cellStyle =
    css
        [ border3 (px 1) solid black
        , overflow hidden
        , textOverflow ellipsis
        , width cellSize
        , height cellSize
        , textAlign center
        ]


winningCellStyle : List (Attribute Msg)
winningCellStyle =
    [ css
        [ backgroundColor winningGreen
        , color white
        , fontWeight bold
        ]
    ]


winningGreen : Color
winningGreen =
    hex "2c962f"


white : Color
white =
    hex "ffffff"


black : Color
black =
    hex "000000"
