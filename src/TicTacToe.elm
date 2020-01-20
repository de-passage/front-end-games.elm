module TicTacToe exposing (Controller(..), Model, Msg(..), init, update, view)

import Css exposing (..)
import Dict exposing (Dict)
import Html.Styled exposing (Attribute, Html, button, div, text)
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


type Position
    = Position Int


type GameState
    = Won Victory
    | Draw
    | OnGoing


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
        |> Maybe.andThen (\p -> players |> Maybe.andThen (\l -> if List.all (samePlayer p) l then Just p else Nothing))
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


type alias HtmlElement =
    List (Attribute Msg) -> List (Html Msg) -> Html Msg


viewCell : HtmlElement -> IndexedCell -> Html Msg
viewCell el cell =
    case cellValue cell of
        Played player ->
            div
                [ css
                    [ displayFlex
                    , width (em 2)
                    , border2 (px 1) solid
                    ]
                ]
                [ symbolFor player ]

        EmptyCell ->
            el
                [ onClick (PlayAt <| cellPosition cell)
                , css
                    [ displayFlex
                    , border2 (px 1) solid
                    ]
                ]
                [  ]


viewRow : HtmlElement -> List IndexedCell -> Html Msg
viewRow f cells =
    div
        [ css
            [ displayFlex
            , height <| em 2
            ]
        ]
    <|
        List.map (viewCell f) cells


viewBoard : HtmlElement -> List (List IndexedCell) -> Html Msg
viewBoard f b =
    div
        [ css
            [ border2 (px 1) solid
            , display inlineBlock
            ]
        ]
        (List.map (viewRow f) b)


viewOnGoing : GameOngoingModel -> Html Msg
viewOnGoing model =
    let
        b =
            boardAsTable model.board
    in
    div []
        [ div [] [ text (playerTurnText model.currentPlayer) ]
        , viewBoard button b
        ]


viewGameOver : GameOverModel -> Html Msg
viewGameOver model =
    let
        b =
            boardAsTable model.board
    in
    div []
        [ div [] [ text (toString model.state) ]
        , viewBoard div b
        ]


view : Model -> Html Msg
view model =
    case model of
        GameOver m ->
            viewGameOver m

        GameOngoing m ->
            viewOnGoing m


update : Msg -> Model -> Model
update msg model =
    case msg of
        Restart ->
            init

        PlayAt pos ->
            case model of
                GameOver _ ->
                    model

                GameOngoing m ->
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


positionToString : Position -> String
positionToString (Position p) =
    let
        x =
            p // 3 + 1

        y =
            modBy 3 p + 1
    in
    "(" ++ String.fromInt p ++ "| " ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ")"


toString : GameState -> String
toString state =
    case state of
        Won ( list, Player i ) ->
            "Player " ++ String.fromInt i ++ " has won! Winning positions are : " ++ (list |> List.map positionToString |> List.intersperse ", " |> String.concat)

        Draw ->
            "It's a draw!"

        _ ->
            "The game is not over yet"


playerTurnText : Player -> String
playerTurnText (Player i) =
    "It is player " ++ String.fromInt i ++ "'s turn to play."
