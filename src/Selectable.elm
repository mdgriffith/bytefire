module Selectable exposing (..)


type alias Selectable a =
    { past : List a
    , current : a
    , upcoming : List a
    }


type Loc
    = Past
    | Current
    | Upcoming


map : (a -> b) -> Selectable a -> Selectable b
map fn selectable =
    { past = List.map fn selectable.past
    , current = fn selectable.current
    , upcoming = List.map fn selectable.upcoming
    }


mapLocation : (Loc -> a -> b) -> Selectable a -> List b
mapLocation fn selectable =
    let
        past =
            List.map (fn Past) selectable.past

        current =
            fn Current selectable.current

        upcoming =
            List.map (fn Upcoming) selectable.past
    in
        past ++ [ current ] ++ upcoming


indexedMapLocation : (Loc -> Int -> a -> b) -> Selectable a -> List b
indexedMapLocation fn selectable =
    let
        past =
            List.indexedMap (fn Past) selectable.past

        pastLength =
            List.length selectable.past

        current =
            fn Current pastLength selectable.current

        upcoming =
            List.indexedMap (\i a -> fn Upcoming (i + pastLength + 1) a) selectable.past
    in
        past ++ [ current ] ++ upcoming
