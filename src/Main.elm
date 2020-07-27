module Main exposing (Model, Msg, init, main, subscriptions, update, view)

import Browser
import Css exposing (..)
import GuessANumber as GAN
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Minesweeper as MS
import Snake as SK
import Tetris as T
import TicTacToe as TTT


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type GameModel
    = TicTacToe TTT.Model
    | Minesweeper MS.Model
    | Snake SK.Model
    | GuessANumber GAN.Model
    | Tetris T.Model


type GameTag
    = TicTacToeT
    | MinesweeperT
    | SnakeT
    | GuessANumberT
    | TetrisT


type alias Model =
    GameModel


type ExtMsg
    = TicTacToeMsg TTT.Msg
    | MinesweeperMsg MS.Msg
    | SnakeMsg SK.Msg
    | GuessANumberMsg GAN.Msg
    | TetrisMsg T.Msg


type Msg
    = ExternalMessage ExtMsg
    | GameChanged GameTag



-- INIT


init : flags -> ( Model, Cmd Msg )
init _ =
    initialModel SnakeT


initialModel : GameTag -> ( GameModel, Cmd Msg )
initialModel tag =
    let
        fromTTTCmd : Cmd TTT.Msg -> Cmd Msg
        fromTTTCmd =
            adaptCmd TicTacToeMsg

        fromMSCmd : Cmd MS.Msg -> Cmd Msg
        fromMSCmd =
            adaptCmd MinesweeperMsg

        fromSKCmd : Cmd SK.Msg -> Cmd Msg
        fromSKCmd =
            adaptCmd SnakeMsg

        fromGANCmd : Cmd GAN.Msg -> Cmd Msg
        fromGANCmd =
            adaptCmd GuessANumberMsg

        fromTCmd : Cmd T.Msg -> Cmd Msg
        fromTCmd =
            adaptCmd TetrisMsg
    in
    case tag of
        MinesweeperT ->
            MS.init |> Tuple.mapBoth Minesweeper fromMSCmd

        TicTacToeT ->
            TTT.init |> Tuple.mapBoth TicTacToe fromTTTCmd

        SnakeT ->
            SK.init |> Tuple.mapBoth Snake fromSKCmd

        GuessANumberT ->
            GAN.init |> Tuple.mapBoth GuessANumber fromGANCmd

        TetrisT ->
            T.init |> Tuple.mapBoth Tetris fromTCmd



-- ADAPTERS


adaptCmd : (msg -> ExtMsg) -> Cmd msg -> Cmd Msg
adaptCmd =
    adaptFunc Cmd.map


adaptFunc : ((b -> Msg) -> a -> c) -> (b -> ExtMsg) -> a -> c
adaptFunc m f a =
    m (f >> ExternalMessage) a


dispatchUpdate :
    ExtMsg
    -> a
    -> (TTT.Msg -> TTT.Model -> a)
    -> (MS.Msg -> MS.Model -> a)
    -> (SK.Msg -> SK.Model -> a)
    -> (GAN.Msg -> GAN.Model -> a)
    -> (T.Msg -> T.Model -> a)
    -> GameModel
    -> a
dispatchUpdate msg default ttt ms sk gan tetris model =
    case msg of
        TicTacToeMsg msg1 ->
            case model of
                TicTacToe m ->
                    ttt msg1 m

                _ ->
                    default

        MinesweeperMsg msg1 ->
            case model of
                Minesweeper m ->
                    ms msg1 m

                _ ->
                    default

        SnakeMsg msg1 ->
            case model of
                Snake m ->
                    sk msg1 m

                _ ->
                    default

        GuessANumberMsg msg1 ->
            case model of
                GuessANumber m ->
                    gan msg1 m

                _ ->
                    default

        TetrisMsg msg1 ->
            case model of
                Tetris m ->
                    tetris msg1 m

                _ ->
                    default


gameType : GameModel -> GameTag
gameType model =
    case model of
        TicTacToe _ ->
            TicTacToeT

        Minesweeper _ ->
            MinesweeperT

        Snake _ ->
            SnakeT

        GuessANumber _ ->
            GuessANumberT

        Tetris _ ->
            TetrisT


isGame : GameTag -> GameModel -> Bool
isGame tag model =
    tag == gameType model


mapUpdate :
    (msg -> model -> ( model, Cmd msg ))
    -> (msg -> ExtMsg)
    -> (model -> GameModel)
    -> msg
    -> model
    -> ( Model, Cmd Msg )
mapUpdate upd toMsg toModel msg model =
    let
        ( m, c ) =
            upd msg model
    in
    ( toModel m, adaptCmd toMsg c )


updateTTT :
    TTT.Msg
    -> TTT.Model
    -> ( Model, Cmd Msg )
updateTTT =
    mapUpdate TTT.update TicTacToeMsg TicTacToe


updateMS :
    MS.Msg
    -> MS.Model
    -> ( Model, Cmd Msg )
updateMS =
    mapUpdate MS.update MinesweeperMsg Minesweeper


updateSK :
    SK.Msg
    -> SK.Model
    -> ( Model, Cmd Msg )
updateSK =
    mapUpdate SK.update SnakeMsg Snake


updateGAN :
    GAN.Msg
    -> GAN.Model
    -> ( Model, Cmd Msg )
updateGAN =
    mapUpdate GAN.update GuessANumberMsg GuessANumber


updateT : T.Msg -> T.Model -> ( Model, Cmd Msg )
updateT =
    mapUpdate T.update TetrisMsg Tetris


noCmd : a -> ( a, Cmd msg )
noCmd a =
    ( a, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExternalMessage msg1 ->
            dispatchUpdate msg1
                (noCmd model)
                updateTTT
                updateMS
                updateSK
                updateGAN
                updateT
                model

        GameChanged newGame ->
            initialModel newGame


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Snake m ->
            Sub.map (SnakeMsg >> ExternalMessage) (SK.subscriptions m)

        _ ->
            Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Games!"
    , body =
        [ tabed model |> toUnstyled
        ]
    }


tabsFor : Model -> Html Msg
tabsFor model =
    let
        games =
            [ ( "Minesweeper", MinesweeperT )
            , ( "TicTacToe", TicTacToeT )
            , ( "Snake", SnakeT )
            , ( "Tetris", TetrisT )

            -- , ( "Guess a number", GuessANumberT )
            ]

        tabStyle tag =
            let
                additional =
                    if isGame tag model then
                        basicTabStyle ++ selectedStyle

                    else
                        basicTabStyle
            in
            marginRight (px 10) :: additional

        onClickEvent tag =
            if not <| isGame tag model then
                [ onClick (GameChanged tag) ]

            else
                []

        mkDiv ( name, tag ) =
            div (onClickEvent tag ++ [ css (tabStyle tag) ]) [ text name ]
    in
    games
        |> List.map mkDiv
        |> div [ css tabHeaderStyle ]


tabed : Model -> Html Msg
tabed model =
    div []
        [ tabsFor model
        , gameView model
        ]


gameView : GameModel -> Html Msg
gameView m =
    let
        adaptHtml : (msg -> ExtMsg) -> Html msg -> Html Msg
        adaptHtml =
            adaptFunc Html.map
    in
    case m of
        TicTacToe model ->
            div []
                [ adaptHtml TicTacToeMsg (TTT.view model) ]

        Minesweeper model ->
            div []
                [ adaptHtml MinesweeperMsg (MS.view model) ]

        Snake model ->
            div [] [ adaptHtml SnakeMsg (SK.view model) ]

        GuessANumber model ->
            div [] [ adaptHtml GuessANumberMsg (GAN.view model) ]

        Tetris model ->
            div [] [ adaptHtml TetrisMsg (T.view model) ]



-- CSS STYLING


white : Color
white =
    rgb 255 255 255


selectedBgColor : Color
selectedBgColor =
    rgb 57 119 219


selectedTextColor : Color
selectedTextColor =
    white


tabBorderColor : Color
tabBorderColor =
    selectedBgColor


tabBorder : Color -> Style
tabBorder c =
    let
        size =
            px 3

        style =
            solid

        b f =
            f size style c
    in
    Css.batch [ b borderTop3, b borderLeft3, b borderRight3 ]


selectedStyle : List Style
selectedStyle =
    [ backgroundColor selectedBgColor
    , color selectedTextColor
    , tabBorderArc
    , tabBorder selectedBgColor
    ]


tabBorderArc : Style
tabBorderArc =
    borderRadius4 (Css.em 0.8) (Css.em 0.8) (Css.em 0) (Css.em 0)


tabHeaderStyle : List Style
tabHeaderStyle =
    [ displayFlex
    , marginBottom (Css.em 1)
    , borderBottom2 (px 1) solid
    , lineHeight (Css.em 2.5)
    , flexWrap Css.wrap
    ]


basicTabStyle : List Style
basicTabStyle =
    [ cursor pointer
    , tabBorder white
    , tabBorderArc
    , padding2 (px 0) (px 10)
    , hover [ tabBorder tabBorderColor ]
    ]
