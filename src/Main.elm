module Main exposing (Model, Msg, init, main, subscriptions, update, view)

import Browser
import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Minesweeper as MS
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


type GameTag
    = TicTacToeT
    | MinesweeperT


type alias Model =
    { gameModel : GameModel
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { gameModel = TicTacToe TTT.init
      }
    , Cmd.none
    )


type ExtMsg
    = TicTacToeMsg TTT.Msg
    | MinesweeperMsg MS.Msg


type Msg
    = ExternalMessage ExtMsg
    | GameChanged GameTag


initialGame : GameTag -> GameModel
initialGame tag =
    case tag of
        MinesweeperT ->
            Minesweeper MS.init

        TicTacToeT ->
            TicTacToe TTT.init


dispatchUpdate :
    ExtMsg
    -> GameModel
    -> a
    -> (TTT.Msg -> TTT.Model -> a)
    -> (MS.Msg -> MS.Model -> a)
    -> a
dispatchUpdate msg model default ttt ms =
    case msg of
        TicTacToeMsg msg1 ->
            tictactoe default (ttt msg1) model

        MinesweeperMsg msg1 ->
            minesweeper default (ms msg1) model


dispatchGame : (TTT.Model -> a) -> (MS.Model -> a) -> GameModel -> a
dispatchGame ttt ms game =
    case game of
        TicTacToe m ->
            ttt m

        Minesweeper m ->
            ms m


minesweeper : a -> (MS.Model -> a) -> GameModel -> a
minesweeper default =
    dispatchGame (always default)


tictactoe : a -> (TTT.Model -> a) -> GameModel -> a
tictactoe default f =
    dispatchGame f (always default)


tictacttoeTag : TTT.Model -> GameTag
tictacttoeTag =
    always TicTacToeT


minesweeperTag : MS.Model -> GameTag
minesweeperTag =
    always MinesweeperT


gameType : GameModel -> GameTag
gameType =
    dispatchGame tictacttoeTag minesweeperTag


isGame : GameTag -> GameModel -> Bool
isGame tag model =
    tag == gameType model


mapUpdate :
    (msg -> model -> ( model, Cmd msg ))
    -> (msg -> ExtMsg)
    -> (model -> GameModel)
    -> Model
    -> msg
    -> model
    -> ( Model, Cmd Msg )
mapUpdate upd toMsg toModel model1 msg model =
    let
        ( m, c ) =
            upd msg model
    in
    ( { model1 | gameModel = toModel m }, Cmd.map (toMsg >> ExternalMessage) c )


updateTTT :
    Model
    -> TTT.Msg
    -> TTT.Model
    -> ( Model, Cmd Msg )
updateTTT =
    mapUpdate TTT.update TicTacToeMsg TicTacToe


updateMS :
    Model
    -> MS.Msg
    -> MS.Model
    -> ( Model, Cmd Msg )
updateMS =
    mapUpdate MS.update MinesweeperMsg Minesweeper


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExternalMessage msg1 ->
            dispatchUpdate msg1 model.gameModel ( model, Cmd.none ) (updateTTT model) (updateMS model)

        GameChanged newGame ->
            ( { model | gameModel = initialGame newGame }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
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
            ]

        tabStyle tag =
            let
                additional =
                    if isGame tag model.gameModel then
                        basicTabStyle ++ selectedStyle

                    else
                        basicTabStyle
            in
            marginRight (px 10) :: additional

        mkDiv ( name, tag ) =
            div [ css (tabStyle tag), onClick (GameChanged tag) ] [ text name ]
    in
    games
        |> List.map mkDiv
        |> div [ css tabHeaderStyle ]


tabed : Model -> Html Msg
tabed model =
    div []
        [ tabsFor model
        , gameView model.gameModel
        ]


gameView : GameModel -> Html Msg
gameView m =
    case m of
        TicTacToe model ->
            div []
                [ text "TicTacToe", Html.map (TicTacToeMsg >> ExternalMessage) (TTT.view model) ]

        Minesweeper model ->
            div []
                [ text "Minesweeper", Html.map (MinesweeperMsg >> ExternalMessage) (MS.view model) ]



-- CSS STYLING


white : Color
white =
    rgb 255 255 255


black : Color
black =
    rgb 0 0 0


selectedBgColor : Color
selectedBgColor =
    rgb 57 119 219


selectedTextColor : Color
selectedTextColor =
    white


tabBorderColor : Color
tabBorderColor =
    rgb 10 15 148


tabBorder : Color -> Style
tabBorder c =
    let
        size =
            px 3

        style =
            solid

        b f = f size style c
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
