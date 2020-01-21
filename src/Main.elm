module Main exposing (Model, Msg, init, main, subscriptions, update, view)

import Browser
import Html.Styled as Html exposing (..)
import TicTacToe as TTT


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { tictactoe : TTT.Model
    }


init : flags -> ( Model, Cmd Msg )
init flags =
    ( { tictactoe = TTT.init }, Cmd.none )


type Msg
    = TicTacToeMsg TTT.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TicTacToeMsg tttmsg ->
            let
                m =
                    TTT.update tttmsg model.tictactoe
            in
            ( { model | tictactoe = m }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Games!"
    , body =
        [ toUnstyled <|
            div []
                [ text "TicTacToe", Html.map (\msg -> TicTacToeMsg msg) <| TTT.view model.tictactoe ]
        ]
    }
