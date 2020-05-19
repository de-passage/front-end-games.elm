module Plane exposing
    ( Coordinates
    , Height(..)
    , Plane
    , Point
    , Row
    , Width(..)
    , X(..)
    , Y(..)
    , at
    , clampAt
    , clampSetAt
    , clampToCoordinates
    , clampToPoint
    , clampUpdateAt
    , coordinatesEqual
    , defaultInitialize
    , down
    , fromArray
    , fromCoordinates
    , fromPoint
    , get
    , getFromCoordinates
    , height
    , initialize
    , left
    , map
    , mapIndexed
    , mapIndexedRow
    , mapRows
    , pointToPlane
    , right
    , set
    , setAt
    , toArray
    , toCoordinates
    , toList
    , up
    , update
    , updateAt
    , width
    , wrapAt
    , wrapDown
    , wrapLeft
    , wrapPointDown
    , wrapPointLeft
    , wrapPointRight
    , wrapPointUp
    , wrapRight
    , wrapSetAt
    , wrapToCoordinates
    , wrapToPoint
    , wrapUp
    , wrapUpdateAt
    )

import Array exposing (Array)


type X
    = X Int


type Y
    = Y Int


type Coordinates
    = C Int


type Width
    = Width Int


type Height
    = Height Int


type Plane a
    = Plane (Array a) Height Width a


type Point a
    = Point Coordinates (Plane a)


type Row a
    = Row (Array a) X Width


initialize : Height -> Width -> (Coordinates -> a) -> Plane a
initialize (Height h) (Width w) f =
    let
        w1 =
            if w < 0 then
                1

            else
                w

        h1 =
            if h < 0 then
                1

            else
                h
    in
    Plane (Array.initialize (h1 * w1) (\i -> f (C i))) (Height h1) (Width w1) (f (C 0))


fromArray : Height -> Width -> a -> Array a -> Maybe (Plane a)
fromArray ((Height h) as hh) ((Width w) as ww) d a =
    let
        tot =
            h * w
    in
    if tot == Array.length a then
        Just <| Plane a hh ww d

    else
        Maybe.Nothing


defaultInitialize : Height -> Width -> a -> Plane a
defaultInitialize h w a =
    initialize h w (always a)


toCoordinates : X -> Y -> Plane a -> Maybe Coordinates
toCoordinates (X x) (Y y) (Plane _ (Height h) (Width w) _) =
    if x < 0 || y < 0 || x >= h || y >= w then
        Nothing

    else
        Just (C (x * w + y))


fromCoordinates : Coordinates -> Plane a -> ( X, Y )
fromCoordinates (C i) (Plane _ _ (Width w) _) =
    ( X (i // w), Y (modBy w i) )


toPoint : X -> Y -> Plane a -> Maybe (Point a)
toPoint (X x) (Y y) (Plane p ((Height h) as hh) ((Width w) as ww) d) =
    if x < 0 || y < 0 || x >= h || y >= w then
        Nothing

    else
        Just (Point (C (x * w + y)) (Plane p hh ww d))


fromPoint : Point a -> ( Coordinates, Plane a )
fromPoint (Point c p) =
    ( c, p )


pointToPlane : Point a -> Plane a
pointToPlane (Point _ p) =
    p


pointToCoordinates : Point a -> Coordinates
pointToCoordinates (Point c _) =
    c


clampToCoordinates : X -> Y -> Plane a -> Coordinates
clampToCoordinates x y p =
    pointToCoordinates (clampToPoint x y p)


clampToPoint : X -> Y -> Plane a -> Point a
clampToPoint (X x) (Y y) (Plane p (Height h) (Width w) d) =
    let
        x1 =
            clamp 0 (h - 1) x

        y1 =
            clamp 0 (w - 1) y
    in
    Point (C (x1 * w + y1)) (Plane p (Height h) (Width w) d)


wrapToCoordinates : X -> Y -> Plane a -> Coordinates
wrapToCoordinates x y p =
    pointToCoordinates (wrapToPoint x y p)


wrapToPoint : X -> Y -> Plane a -> Point a
wrapToPoint (X x) (Y y) (Plane p (Height h) (Width w) d) =
    let
        x1 =
            if x < 0 || x >= h then
                modBy h x

            else
                x

        y1 =
            if y < 0 || y >= w then
                modBy w y

            else
                y
    in
    Point (C (x1 * w + y1)) (Plane p (Height h) (Width w) d)


getFromCoordinates : Coordinates -> Plane a -> Maybe a
getFromCoordinates (C c) (Plane a _ _ _) =
    Array.get c a


at : Point a -> a
at (Point (C c) (Plane a _ _ d)) =
    case Array.get c a of
        Just r ->
            r

        Nothing ->
            d


get : X -> Y -> Plane a -> Maybe a
get (X x) (Y y) (Plane p _ (Width w) _) =
    Array.get (x * w + y) p


clampAt : X -> Y -> Plane a -> a
clampAt x y p =
    at (clampToPoint x y p)


wrapAt : X -> Y -> Plane a -> a
wrapAt x y p =
    at (wrapToPoint x y p)


set : Coordinates -> a -> Plane a -> Plane a
set (C c) a (Plane p h w d) =
    Plane (Array.set c a p) h w d


setAt : X -> Y -> a -> Plane a -> Maybe (Plane a)
setAt x y a p =
    toCoordinates x y p
        |> Maybe.map (\c -> set c a p)


wrapSetAt : X -> Y -> a -> Plane a -> Plane a
wrapSetAt x y a p =
    set (wrapToCoordinates x y p) a p


clampSetAt : X -> Y -> a -> Plane a -> Plane a
clampSetAt x y a p =
    set (clampToCoordinates x y p) a p


update : (a -> a) -> Point a -> Plane a
update f (Point (C c) (Plane p h w d)) =
    case Array.get c p of
        Nothing ->
            Plane p h w d

        Just a ->
            Plane (Array.set c (f a) p) h w d


updateAt : X -> Y -> (a -> a) -> Plane a -> Maybe (Plane a)
updateAt x y f p =
    toPoint x y p
        |> Maybe.map (\c -> update f c)


clampUpdateAt : X -> Y -> (a -> a) -> Plane a -> Plane a
clampUpdateAt x y f p =
    update f (clampToPoint x y p)


wrapUpdateAt : X -> Y -> (a -> a) -> Plane a -> Plane a
wrapUpdateAt x y f p =
    update f (wrapToPoint x y p)


height : Plane a -> Height
height (Plane _ h _ _) =
    h


width : Plane a -> Width
width (Plane _ _ w _) =
    w


left : X -> Y -> Plane a -> Maybe (Point a)
left x (Y y) =
    toPoint x (Y (y - 1))


right : X -> Y -> Plane a -> Maybe (Point a)
right x (Y y) =
    toPoint x (Y (y + 1))


up : X -> Y -> Plane a -> Maybe (Point a)
up (X x) y =
    toPoint (X (x - 1)) y


down : X -> Y -> Plane a -> Maybe (Point a)
down (X x) y =
    toPoint (X (x + 1)) y


wrapLeft : X -> Y -> Plane a -> Point a
wrapLeft x (Y y) =
    wrapToPoint x (Y (y - 1))


wrapRight : X -> Y -> Plane a -> Point a
wrapRight x (Y y) =
    wrapToPoint x (Y (y + 1))


wrapUp : X -> Y -> Plane a -> Point a
wrapUp (X x) y =
    wrapToPoint (X (x - 1)) y


wrapDown : X -> Y -> Plane a -> Point a
wrapDown (X x) y =
    wrapToPoint (X (x + 1)) y


wrapPointUp : Point a -> Point a
wrapPointUp (Point c plane) =
    let
        ( x, y ) =
            fromCoordinates c plane
    in
    wrapUp x y plane


wrapPointDown : Point a -> Point a
wrapPointDown (Point c plane) =
    let
        ( x, y ) =
            fromCoordinates c plane
    in
    wrapDown x y plane


wrapPointLeft : Point a -> Point a
wrapPointLeft (Point c plane) =
    let
        ( x, y ) =
            fromCoordinates c plane
    in
    wrapLeft x y plane


wrapPointRight : Point a -> Point a
wrapPointRight (Point c plane) =
    let
        ( x, y ) =
            fromCoordinates c plane
    in
    wrapRight x y plane


toArray : Plane a -> Array (Point a)
toArray (Plane b h w d) =
    Array.indexedMap (\i _ -> Point (C i) (Plane b h w d)) b


toList : Plane a -> List (Point a)
toList =
    toArray >> Array.toList


mapRows : (Row a -> b) -> Plane a -> List b
mapRows f (Plane b _ (Width w) _) =
    let
        loop curr arr acc =
            if Array.length arr <= curr then
                List.reverse acc

            else
                loop (curr + w) arr (f (Row (Array.slice curr (curr + w) arr) (X (curr // w)) (Width w)) :: acc)
    in
    loop 0 b []


mapIndexedRow : (Coordinates -> a -> b) -> Row a -> Array b
mapIndexedRow f (Row arr (X x) (Width w)) =
    Array.indexedMap (\y v -> f (C (x * w + y)) v) arr


map : (a -> b) -> Plane a -> Plane b
map f (Plane b h w d) =
    Array.map f b
        |> (\b1 -> Plane b1 h w (f d))


mapIndexed : (Coordinates -> a -> b) -> Plane a -> Plane b
mapIndexed f (Plane b h w d) =
    Array.indexedMap (\c v -> f (C c) v) b
        |> (\b1 -> Plane b1 h w (f (C -1) d))


coordinatesEqual : Coordinates -> Coordinates -> Bool
coordinatesEqual (C a) (C b) =
    a == b
