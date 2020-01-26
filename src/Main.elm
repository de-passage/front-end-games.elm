module Main exposing (Model, Msg, init, main, subscriptions, update, view)

import Browser
import Css exposing (..)
import Function
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
    let
        ( model, command ) =
            MS.init
    in
    ( { gameModel = Minesweeper model
      }
    , fromMSCmd command
    )


adaptCmd : (msg -> ExtMsg) -> Cmd msg -> Cmd Msg
adaptCmd = adaptFunc Cmd.map

adaptHtml : (msg -> ExtMsg) -> Html msg -> Html Msg
adaptHtml = adaptFunc Html.map

adaptFunc : ((b -> Msg) -> a -> c) -> (b -> ExtMsg) -> a -> c
adaptFunc m f a = m (f >> ExternalMessage) a


fromTTTCmd : Cmd TTT.Msg -> Cmd Msg
fromTTTCmd =
    adaptCmd TicTacToeMsg


fromMSCmd : Cmd MS.Msg -> Cmd Msg
fromMSCmd =
    adaptCmd MinesweeperMsg

fromTTTHtml : Html TTT.Msg -> Html Msg
fromTTTHtml = 
    adaptHtml TicTacToeMsg

fromMSHtml : Html MS.Msg -> Html Msg
fromMSHtml = 
    adaptHtml MinesweeperMsg

type ExtMsg
    = TicTacToeMsg TTT.Msg
    | MinesweeperMsg MS.Msg


type Msg
    = ExternalMessage ExtMsg
    | GameChanged GameTag


initialGame : GameTag -> ( GameModel, Cmd Msg )
initialGame tag =
    case tag of
        MinesweeperT ->
            MS.init |> Tuple.mapBoth Minesweeper fromMSCmd

        TicTacToeT ->
            TTT.init |> Tuple.mapBoth TicTacToe fromTTTCmd


dispatchUpdate :
    ExtMsg
    -> a
    -> (TTT.Msg -> TTT.Model -> a)
    -> (MS.Msg -> MS.Model -> a)
    -> GameModel
    -> a
dispatchUpdate msg default ttt ms =
    case msg of
        TicTacToeMsg msg1 ->
            tictactoe default (ttt msg1)

        MinesweeperMsg msg1 ->
            minesweeper default (ms msg1)


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
    ( { model1 | gameModel = toModel m }, adaptCmd toMsg c )


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


noCmd : a -> ( a, Cmd msg )
noCmd a =
    ( a, Cmd.none )


getGame : Model -> GameModel
getGame m =
    m.gameModel


setGame : Model -> GameModel -> Model
setGame m g =
    { m | gameModel = g }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExternalMessage msg1 ->
            Function.lift4 (dispatchUpdate msg1) noCmd updateTTT updateMS getGame <| model

        GameChanged newGame ->
            initialGame newGame
                |> Tuple.mapFirst (setGame model)


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

        onClickEvent tag =
            if not <| isGame tag model.gameModel then
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
        , gameView model.gameModel
        ]


gameView : GameModel -> Html Msg
gameView m =
    case m of
        TicTacToe model ->
            div []
                [ text "TicTacToe", fromTTTHtml (TTT.view model) ]

        Minesweeper model ->
            div []
                [ text "Minesweeper", fromMSHtml (MS.view model) ]



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
