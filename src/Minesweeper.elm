module Minesweeper exposing (Model, Msg, init, update, view)

import Array exposing (Array)
import Dict exposing (Dict)
import Html.Events.Extra
import Html.Styled exposing (Attribute, Html, button, div, input, text)
import Html.Styled.Attributes exposing (css, type_, value)
import Html.Styled.Events exposing (onClick)
import Random


onChange =
    Html.Styled.Attributes.fromUnstyled << Html.Events.Extra.onChange


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
    , list : Array Int
    , min : Int
    , max : Int
    , count : Int
    }


type Msg
    = PlayedAt ( Int, Int )
    | Restart ( Int, Int, Int )
    | RequestedNewList
    | NewListGenerated (Array Int)
    | MaxChanged Int
    | MinChanged Int
    | CountChanged Int


number : Int -> List (Attribute msg) -> List (Html msg) -> Html msg
number val attr content =
    input (type_ "number" :: (value <| String.fromInt val) :: attr) content


tryMsg : (Int -> Msg) -> Int -> String -> Msg
tryMsg m i =
    String.toInt
        >> Maybe.withDefault i
        >> m


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick RequestedNewList ] [ text "Generate" ]
        , text
            ((\l -> "[" ++ l ++ "]") <|
                String.concat <|
                    List.intersperse ", " <|
                        List.map String.fromInt <|
                            Array.toList model.list
            )
        , number model.min [ onChange (tryMsg MinChanged model.min) ] []
        , number model.max [ onChange (tryMsg MaxChanged model.max) ] []
        , number model.count [ onChange (tryMsg CountChanged model.count) ] []
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NewListGenerated array ->
            ( { model | list = array }, Cmd.none )

        RequestedNewList ->
            ( model, Random.generate NewListGenerated (reservoirSample model.count (Array.initialize (model.max - model.min + 1) (\i -> i + model.min))) )

        MaxChanged m ->
            ( { model | max = m }, Cmd.none )

        MinChanged m ->
            ( { model | min = m }, Cmd.none )

        CountChanged c ->
            ( { model | count = c }, Cmd.none )

        _ ->
            ( model, Cmd.none )


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
    , list = Array.empty
    , min = 0
    , max = 100
    , count = 10
    }


{-| Adapted from wikipedia <https://en.wikipedia.org/wiki/Reservoir_sampling>
-}
reservoirSample : Int -> Array a -> Random.Generator (Array a)
reservoirSample n source =
    let
        max =
            Array.length source

        randomF : Random.Generator Float
        randomF =
            Random.float 0 1

        result =
            Array.fromList (List.take n <| Array.toList source)

        w : Random.Generator Float
        w =
            randomF
                |> Random.map (\r -> e ^ (logBase e r / toFloat (n - 1)))

        next : Int -> Float -> Random.Generator Int
        next i x =
            randomF |> Random.map (\r -> 1 + i + floor (logBase e r / logBase e (1 - x)))

        randomI : Random.Generator Int
        randomI =
            Random.int 0 (n - 1)

        repl i list k =
            Array.get i source
                |> Maybe.map (\e -> Array.set k e list)
                |> Maybe.withDefault list

        loop : Int -> Array a -> Float -> Random.Generator (Array a)
        loop i list x =
            if i >= max then
                Random.constant list

            else
                randomI
                    |> Random.andThen
                        (\k ->
                            next i x
                                |> Random.andThen
                                    (\j ->
                                        w
                                            |> Random.andThen (\y -> loop j (repl i list k) (x * y))
                                    )
                        )
    in
    if n <= 0 || max == 0 then
        Random.constant Array.empty

    else if n == 1 then
        Random.int 0 (max - 1)
            |> Random.map
                (\i ->
                    Array.get i source
                        |> Maybe.map (\x -> Array.fromList [x])
                        |> Maybe.withDefault Array.empty
                )

    else
        w
            |> Random.andThen
                (\x ->
                    next (n - 1) x
                        |> Random.andThen (\j -> loop j result x)
                )
