module Minesweeper exposing (init, update, view, Msg, Model)

import Dict exposing (Dict)
import Html.Styled exposing (Html, div)


type Cell
    = Mine
    | Empty


type alias Board =
    Dict ( Int, Int ) Cell


type GameStatus
    = Ongoing
    | Won
    | Lost ( Int, Int )


type alias Model =
    { cells : Board
    , boardWidth : Int
    , boardHeight : Int
    , status : GameStatus
    }


type Msg = PlayedAt (Int, Int) | Restart (Int, Int, Int)

view : Model -> Html msg
view _ =
    div [] []


update : Model -> (Model, Cmd Msg)
update _ = (init, Cmd.none)
    

init : Model
init =
    { cells =
        Dict.fromList
            [ ( ( 0, 0 ), Mine )
            , ( ( 0, 1 ), Empty )
            , ( ( 0, 2 ), Empty )
            , ( ( 1, 0 ), Empty )
            , ( ( 1, 1 ), Empty )
            , ( ( 1, 2 ), Empty )
            , ( ( 2, 0 ), Empty )
            , ( ( 2, 1 ), Mine )
            , ( ( 2, 2 ), Empty )
            ]
    , boardWidth = 3
    , boardHeight = 3
    , status = Ongoing 
    }
