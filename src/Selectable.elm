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


fromList : List a -> Maybe (Selectable a)
fromList list =
    case list of
        [] ->
            Nothing

        start :: remain ->
            Just
                { past = []
                , current = start
                , upcoming = remain
                }


singleton : a -> Selectable a
singleton current =
    { past = []
    , current = current
    , upcoming = []
    }


length : Selectable a -> Int
length { past, current, upcoming } =
    List.length past + 1 + List.length upcoming


select : Int -> Selectable a -> Selectable a
select i ({ past, current, upcoming } as x) =
    let
        pastLength =
            List.length past

        upcomingCount =
            List.length upcoming

        full =
            past ++ [ current ] ++ upcoming

        normalizedIndex =
            if i >= List.length full then
                rem i (List.length full)
            else
                i
    in
        { past = List.take normalizedIndex full
        , current =
            List.drop normalizedIndex full
                |> List.head
                |> Maybe.withDefault current
        , upcoming = List.drop (normalizedIndex + 1) full
        }


next : Selectable a -> Selectable a
next ({ past, current, upcoming } as selected) =
    case upcoming of
        [] ->
            selected

        nxt :: remaining ->
            { past = past ++ [ current ]
            , current = nxt
            , upcoming = remaining
            }


atEnd : Selectable a -> Bool
atEnd { upcoming } =
    List.isEmpty upcoming


first : Selectable a -> a
first { past, current } =
    case past of
        [] ->
            current

        fst :: _ ->
            fst


nextCycledIndex : Selectable a -> Int
nextCycledIndex { past, current, upcoming } =
    let
        upcomingCount =
            List.length upcoming
    in
        if upcomingCount == 0 then
            0
        else
            List.length past + 1


currentIndex : Selectable a -> Int
currentIndex { past } =
    List.length past


mapCurrent : (a -> a) -> Selectable a -> Selectable a
mapCurrent fn selectable =
    { past = selectable.past
    , current = fn selectable.current
    , upcoming = selectable.upcoming
    }


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
            List.indexedMap (\i a -> fn Upcoming (i + pastLength + 1) a) selectable.upcoming
    in
        past ++ [ current ] ++ upcoming
