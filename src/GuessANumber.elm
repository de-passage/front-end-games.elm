module GuessANumber exposing (..)

import Css exposing (..)
import CustomElements exposing (number)
import Html.Styled exposing (Attribute, Html, div, text)
import Html.Styled.Attributes as Attributes exposing (css, max, min, type_, value)



-- TYPES


type GameStatus
    = Runnning TryCount
    | Won Int
    | Stopped


type alias Model =
    { upper : UpperBound
    , lower : LowerBound
    , current : CurrentTry
    , status : GameStatus
    }


type Msg
    = NumberTried CurrentTry
    | LowerChanged LowerBound
    | UpperChanged UpperBound
    | Restart
    | Stop


type LowerBound
    = LB Int


type UpperBound
    = UB Int


type CurrentTry
    = Try Int


type TryCount
    = TC Int



-- INIT


initialModel : Model
initialModel =
    { upper = UB 100
    , lower = LB 0
    , current = Try 0
    , status = Stopped
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [] [ text "Hello world!" ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mdl =
    case msg of
        LowerChanged l ->
            ( { mdl | lower = l }, Cmd.none )

        NumberTried n ->
            ( mdl, Cmd.none )

        Restart ->
            ( mdl, Cmd.none )

        UpperChanged u ->
            ( mdl, Cmd.none )

        Stop ->
            ( mdl, Cmd.none )
