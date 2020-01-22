module TicTacToe exposing (Model, Msg(..), init, update, view)

import Css exposing (..)
import Dict exposing (Dict)
import Html.Styled exposing (Attribute, Html, button, div, fieldset, input, label, table, tbody, td, text, tr)
import Html.Styled.Attributes exposing (checked, css, name, type_)
import Html.Styled.Events exposing (onClick, onInput)
import List.Extra as List
import Maybe.Extra as Maybe
import Process
import Task
import Tuple


type Either a b
    = Left a
    | Right b


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


type alias Model =
    { board : Board
    , state : GameState
    , options : GameOptions
    , expectingRemotePlay : Bool
    }


type alias PlayerInformation =
    { symbol : Char
    , controller : Controller
    }


type GameOptions
    = GameOptions ( PlayerInformation, PlayerInformation )


type Controller
    = Human
    | Computer


type Msg
    = PlayedAt Position Player
    | Restart
    | Surrendered Player
    | OptionChanged GameOptions


type Position
    = Position Int


type GameState
    = Won Victory
    | Draw
    | OnGoing Player
    | Surrender Player
    | WaitingFor Player
    | Stopped


type alias VictoryStatus =
    Either Victory (List Position)


type alias IndexedCell =
    ( Position, Cell )


type alias Victory =
    ( List Position, Player )



-- UTILITIES


isRunning : GameState -> Bool
isRunning state =
    case state of
        OnGoing _ ->
            True

        WaitingFor _ ->
            True

        _ ->
            False


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


isControlledBy : Controller -> GameOptions -> Player -> Bool
isControlledBy ctrl =
    dispatchOnPlayer (\c -> c.controller == ctrl)


setController : Controller -> GameOptions -> Player -> GameOptions
setController contr opts player =
    mapOnPlayer (\r -> { r | controller = contr }) opts player


dispatchOnPlayer : (PlayerInformation -> a) -> GameOptions -> Player -> a
dispatchOnPlayer f (GameOptions ( a, b )) (Player i) =
    if i == 1 then
        f a

    else
        f b


mapOnPlayer : (PlayerInformation -> PlayerInformation) -> GameOptions -> Player -> GameOptions
mapOnPlayer f (GameOptions ( a, b )) (Player i) =
    if i == 1 then
        GameOptions ( f a, b )

    else
        GameOptions ( a, f b )


isEmpty : Cell -> Bool
isEmpty c =
    case c of
        EmptyCell ->
            True

        _ ->
            False


cellValue : IndexedCell -> Cell
cellValue =
    Tuple.second


cellPosition : IndexedCell -> Position
cellPosition =
    Tuple.first



-- COMMANDS


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (always msg)



-- GAME LOGIC


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


availablePositions : Board -> List Position
availablePositions (Board cells) =
    cells
        |> Dict.toList
        |> List.filter (Tuple.second >> isEmpty)
        |> List.map (Tuple.first >> Position)


allSamePlayer : Dict Int Cell -> Player -> List Int -> Maybe (List Position)
allSamePlayer dict player pos =
    let
        players =
            Maybe.combine <| List.map (\i -> Dict.get i dict |> Maybe.andThen cellPlayer) pos
    in
    players
        |> Maybe.andThen
            (\l ->
                if List.all (samePlayer player) l then
                    Just (List.map Position pos)

                else
                    Nothing
            )


winConditions : List (List Int)
winConditions =
    [ [ 0, 3, 6 ], [ 1, 4, 7 ], [ 2, 5, 8 ], [ 0, 1, 2 ], [ 3, 4, 5 ], [ 6, 7, 8 ], [ 0, 4, 8 ], [ 2, 4, 6 ] ]


nextPlayer : Player -> Player
nextPlayer player =
    if samePlayer player player1 then
        player2

    else
        player1


gameState : Board -> Player -> VictoryStatus
gameState (Board cells) player =
    winConditions
        |> List.map (allSamePlayer cells player)
        |> List.find Maybe.isJust
        |> Maybe.join
        |> Maybe.map (\p -> Left ( p, player ))
        |> Maybe.withDefault (Right (availablePositions (Board cells)))



-- AI


type alias Depth =
    Int


type alias Score =
    Int


type alias Move =
    ( Position, Depth )


defeat : Score
defeat =
    -(2 ^ 32)


victory : Score
victory =
    2 ^ 32


draw : Score
draw =
    0


computeNextMove : Board -> Player -> Msg
computeNextMove board player =
    -- compute the list of best moves from the current board for the current player
    bestMoves board player 0
        -- extract the list of positions from the next move
        |> Tuple.second
        -- get the one with minimum depth (fastest victory)
        |> List.minimumBy Tuple.second
        -- take the position
        |> Maybe.map Tuple.first
        -- create a message to play a this position
        |> Maybe.map (\p -> PlayedAt p player)
        -- if we failed at any point (shouldn't happen if the algo is correct), give up
        |> Maybe.withDefault (Surrendered player)


{-| Computes the score of a the best outcome of a given move on a given board by a given player. A higher score means
better chances of victory. The depth given in second position is the number of moves needed to
reach the best computed outcome. Invalid moves return (draw, 0).
-}
minMax : Board -> Player -> Position -> Depth -> ( Score, Depth )
minMax board player position depth =
    case play board position player of
        -- garbage in, garbage out
        Nothing ->
            ( draw, 0 )

        Just boardAtNextMove ->
            case gameState boardAtNextMove player of
                -- we won
                Left _ ->
                    ( victory, depth )

                Right status ->
                    if List.isEmpty status then
                        ( draw, 0 )

                    else
                        -- the game goes on
                        let
                            -- get the best move of the opponent
                            ( opponentScore, opponentMoves ) =
                                bestMoves boardAtNextMove (nextPlayer player) depth
                        in
                        opponentMoves
                            |> List.map Tuple.second
                            -- we do not care about the exact move, only what the best path to victory is
                            |> List.minimum
                            |> Maybe.map (\d -> ( opponentScore, d ))
                            -- failed because no element in the list == draw
                            |> Maybe.withDefault ( draw, depth )


{-| Gets the fastest move to victory. Returns the pair of the score of the best moves and the list of
corresponding positions on the board. If there is no available move, returns (draw, [])
-}
bestMoves : Board -> Player -> Depth -> ( Score, List Move )
bestMoves board player depth =
    List.foldl (aggregate board player depth) ( defeat, [] ) (availablePositions board)


aggregate : Board -> Player -> Depth -> Position -> ( Score, List Move ) -> ( Score, List Move )
aggregate board player depth position ( currentMax, possibilities ) =
    let
        ( moveValue, moveDepth ) =
            minMax board player position (depth + 1)
    in
    if moveValue > currentMax then
        ( moveValue, [ ( position, moveDepth ) ] )

    else if moveValue == currentMax then
        ( currentMax, ( position, moveDepth ) :: possibilities )

    else
        ( currentMax, possibilities )



-- VIEW


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


symbolFor : Player -> GameOptions -> Html msg
symbolFor player (GameOptions ( p1, p2 )) =
    text <|
        if player == player1 then
            String.fromChar p1.symbol

        else
            String.fromChar p2.symbol


viewCell : Model -> IndexedCell -> Html Msg
viewCell model cell =
    let
        styledTd attributes =
            td (cellStyle :: attributes)
    in
    case cellValue cell of
        Played player ->
            styledTd (decorateVictory model.state cell) [ symbolFor player model.options ]

        EmptyCell ->
            case model.state of
                OnGoing player ->
                    styledTd [ onClick (PlayedAt (cellPosition cell) player) ] []

                _ ->
                    styledTd [] []


viewRow : Model -> List IndexedCell -> Html Msg
viewRow m cells =
    tr [] <| List.map (viewCell m) cells


viewBoard : Model -> List (List IndexedCell) -> Html Msg
viewBoard m b =
    table [ tableStyle ] [ tbody [] (List.map (viewRow m) b) ]


viewGame : Model -> List (Html Msg) -> Html Msg
viewGame model control =
    div
        [ css
            [ margin auto
            , maxWidth fitContent
            ]
        ]
        [ div [] control
        , viewBoard model (boardAsTable model.board)
        ]


decorateVictory : GameState -> IndexedCell -> List (Attribute Msg)
decorateVictory state ( cellIndex, _ ) =
    case state of
        Won ( positions, _ ) ->
            if Maybe.isJust (List.find (samePosition cellIndex) positions) then
                winningCellStyle

            else
                []

        _ ->
            []


gameStateToViewElements : GameState -> ( String, Msg, String )
gameStateToViewElements state =
    case state of
        Won ( _, Player i ) ->
            ( "Player " ++ String.fromInt i ++ " has won!"
            , Restart
            , "Restart"
            )

        Draw ->
            ( "It's a draw!"
            , Restart
            , "Restart"
            )

        OnGoing (Player i) ->
            ( "It is player " ++ String.fromInt i ++ "'s turn to play."
            , Surrendered (Player i)
            , "Surrender"
            )

        Surrender (Player i) ->
            ( "Player " ++ String.fromInt i ++ " surrendered..."
            , Restart
            , "Restart"
            )

        Stopped ->
            ( "Select the desired options and hit Start to begin.", Restart, "Start" )

        WaitingFor (Player i) ->
            ( "Player " ++ String.fromInt i ++ " is thinking.", Restart, "Restart" )


controllerSelection : Model -> String -> Player -> Html Msg
controllerSelection model lbl player =
    let
        p f =
            f model.options player

        setOpts c =
            OptionChanged (p (setController c))

        isCtrldBy c =
            p (isControlledBy c)

        mkRadio n c =
            radio n lbl (isCtrldBy c) (setOpts c)
    in
    Html.Styled.fieldset []
        [ label [] [ text lbl ]
        , mkRadio "Human" Human
        , mkRadio "Computer" Computer
        ]


radio : String -> String -> Bool -> Msg -> Html Msg
radio value group isChecked msg =
    label []
        [ input [ type_ "radio", name group, onInput (always msg), checked isChecked ] []
        , text value
        ]


viewControlBoard : Model -> List (Html Msg)
viewControlBoard model =
    let
        ( logText, buttonEvent, buttonText ) =
            gameStateToViewElements model.state

        controls =
            if not (isRunning model.state) then
                [ controllerSelection model "Player 1" player1
                , controllerSelection model "Player 2" player2
                ]

            else
                []
    in
    [ div []
        (div [] [ text logText ]
            :: controls
            ++ [ div [ buttonDivStyle ]
                    [ button [ onClick buttonEvent ] [ text buttonText ]
                    ]
               ]
        )
    ]


view : Model -> Html Msg
view model =
    viewGame model (viewControlBoard model)



-- UPDATE


delayPlay : Board -> Player -> Cmd Msg
delayPlay b p =
    delay 1000 (computeNextMove b p)


getMove : GameOptions -> Board -> Player -> ( GameState, Cmd Msg )
getMove options board player =
    if isControlledBy Human options player then
        ( OnGoing player, Cmd.none )

    else
        ( WaitingFor player, delay 1000 (computeNextMove board player) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Restart ->
            let
                ( state, cmd ) =
                    getMove model.options emptyBoard player1
            in
            ( { model | board = emptyBoard, state = state }, cmd )

        Surrendered player ->
            ( { model | state = Surrender player }, Cmd.none )

        OptionChanged options ->
            ( { model | options = options }, Cmd.none )

        PlayedAt pos player ->
            if not (isRunning model.state) then
                ( model, Cmd.none )

            else
                case play model.board pos player of
                    Nothing ->
                        ( model, Cmd.none )

                    Just board ->
                        case gameState board player of
                            Left v ->
                                ( { model | board = board, state = Won v }, Cmd.none )

                            Right positions ->
                                let
                                    next =
                                        nextPlayer player

                                    ( state, cmd ) =
                                        if List.isEmpty positions then
                                            ( Draw, Cmd.none )

                                        else
                                            getMove model.options board next
                                in
                                ( { model | board = board, state = state }, cmd )



-- INIT


init : Model
init =
    { options = defaultOptions, board = emptyBoard, state = Stopped, expectingRemotePlay = False }


defaultOptions : GameOptions
defaultOptions =
    GameOptions ( { symbol = 'X', controller = Human }, { symbol = 'O', controller = Computer } )


emptyBoard : Board
emptyBoard =
    List.repeat 9 EmptyCell |> List.indexedMap Tuple.pair |> Dict.fromList |> Board



-- STYLES


tableStyle : Attribute Msg
tableStyle =
    css
        [ tableLayout fixed
        , width boardSize
        , height boardSize
        , border2 (px 1) solid
        , borderSpacing (px 0)
        , margin auto
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


buttonDivStyle : Attribute Msg
buttonDivStyle =
    css
        [ margin (px 5)
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


refSize : Float
refSize =
    15


boardSize : Em
boardSize =
    em refSize


cellSize : Em
cellSize =
    em (refSize / 3)
