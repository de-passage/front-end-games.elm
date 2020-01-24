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
    | GameChanged GameModel


dispatch :
    ExtMsg
    -> Model
    -> a
    -> (TTT.Msg -> TTT.Model -> a)
    -> (MS.Msg -> MS.Model -> a)
    -> a
dispatch msg model default ttt ms =
    case msg of
        TicTacToeMsg msg1 ->
            case model.gameModel of
                TicTacToe model1 ->
                    ttt msg1 model1

                _ ->
                    default

        MinesweeperMsg msg1 ->
            case model.gameModel of
                Minesweeper model1 ->
                    ms msg1 model1

                _ ->
                    default


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
            dispatch msg1 model ( model, Cmd.none ) (updateTTT model) (updateMS model)

        GameChanged newGame ->
            ( {model | gameModel = newGame }, Cmd.none )
        


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
            [ ( "Minesweeper", Minesweeper MS.init )
            , ( "TicTacToe", TicTacToe TTT.init )
            ]

        style =
            [ displayFlex ]

        tabStyle =
            []

        mkDiv ( name, initModel ) =
            div [ css tabStyle, onClick (GameChanged initModel) ] [ text name ]
    in
    games
        |> List.map mkDiv
        |> div [ css style ]


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
