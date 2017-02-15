module Model exposing (..)

import Selectable exposing (Selectable)
import Grid exposing (Grid)
import Time exposing (Time)


type alias Model =
    { grid : Grid
    , running : Bool
    , time : Time
    , mode : Mode
    , width : Int
    , height : Int
    , levels : Selectable Level
    }


type alias Level =
    { path : Path
    , map : Map
    , items : List Item
    , functions : Selectable Function
    , stack : List StackLevel
    , conditionalPrepared : Maybe ItemType
    , allowed : AllowedInstructions
    }


type StackLevel
    = StackLevel Int Int


type Mode
    = Playing
    | Executing Int Time
    | Paused
    | Success Time
    | GameFinished
    | Failed Time Why


type Why
    = StackOverflow
    | OutOfBounds
    | NoInstructions


type Map
    = Map (List LineSegment)


type LineSegment
    = Seg Coords Coords


type ItemType
    = Node
    | Square
    | Circle


type alias Item =
    { x : Int
    , y : Int
    , kind : ItemType
    }


type alias Coords =
    { x : Int, y : Int }


type Path
    = Path Coords (List Direction)


type Direction
    = Left
    | Right
    | Up
    | Down


type AllowedInstructions
    = AllowMove
    | AllowMoveFn
    | AllowMoveFnIf
    | AllowAll


allInstructions : List Instruction
allInstructions =
    [ Move Up
    , Move Right
    , Move Down
    , Move Left
    , Call One
    , Call Two
    , Call Three
    ]


getAllowedInstructions : AllowedInstructions -> Int -> Maybe ItemType -> List Instruction
getAllowedInstructions allowed fnCount conditional =
    let
        moves =
            [ Move Up
            , Move Right
            , Move Down
            , Move Left
            ]

        makeConditional doThing =
            case doThing of
                If _ _ ->
                    doThing

                _ ->
                    case conditional of
                        Nothing ->
                            doThing

                        Just condition ->
                            If condition doThing

        functions count =
            if fnCount == 1 then
                [ Call One ]
            else if fnCount == 2 then
                [ Call One, Call Two ]
            else if fnCount == 3 then
                [ Call One, Call Two, Call Three ]
            else
                []

        ifInstruction =
            [ If Square DoNothing, If Circle DoNothing ]
    in
        case allowed of
            AllowMove ->
                List.map makeConditional moves

            AllowMoveFn ->
                List.map makeConditional <| moves ++ functions fnCount

            AllowMoveFnIf ->
                List.map makeConditional <| moves ++ functions fnCount ++ ifInstruction

            AllowAll ->
                List.map makeConditional <| moves ++ functions fnCount ++ ifInstruction


type Instruction
    = Move Direction
    | Call FnIndex
    | If ItemType Instruction
    | DoNothing


type FnIndex
    = One
    | Two
    | Three


{-| Problem with making a game about programming is that you get overlapping words :/
-}
type alias Function =
    { instructions : List (Maybe Instruction)
    }


readyToExecute : Function -> Bool
readyToExecute fn =
    List.all (\x -> x /= Nothing) fn.instructions


resetFunctions : Function -> Function
resetFunctions fn =
    { fn | instructions = List.repeat (List.length fn.instructions) Nothing }


removeLatest : Function -> Function
removeLatest fn =
    let
        replace x ( removed, remain ) =
            if removed then
                ( removed, x :: remain )
            else
                case x of
                    Nothing ->
                        ( removed, x :: remain )

                    Just _ ->
                        ( True, Nothing :: remain )

        newInstructions =
            List.foldr replace ( False, [] ) fn.instructions
                |> Tuple.second
    in
        { fn | instructions = newInstructions }


replaceFirstNothing : Instruction -> Function -> Function
replaceFirstNothing instruction fn =
    let
        replace x ( replaced, remain ) =
            if replaced then
                ( replaced, x :: remain )
            else if x == Nothing then
                ( True, Just instruction :: remain )
            else
                ( replaced, x :: remain )

        newInstructions =
            List.foldl replace ( False, [] ) fn.instructions
                |> Tuple.second
                |> List.reverse
    in
        { fn | instructions = newInstructions }


isFirstNothing : Int -> Function -> Bool
isFirstNothing i fn =
    let
        isNothing x ( found, j ) =
            if found then
                ( found, j )
            else if x == Nothing then
                ( True, j )
            else
                ( False, j + 1 )

        ( found, index ) =
            List.foldl isNothing ( False, 0 ) fn.instructions
    in
        found && index == i


overlapping : Coords -> List { a | x : Int, y : Int } -> Bool
overlapping coord all =
    List.any (matching coord) all


matching : { a | x : Int, y : Int } -> { b | x : Int, y : Int } -> Bool
matching p1 p2 =
    p1.x == p2.x && p1.y == p2.y


overlappingSegment : LineSegment -> List LineSegment -> Bool
overlappingSegment (Seg p1 p2) all =
    List.any
        (\(Seg point1 point2) ->
            (matching p1 point1 && matching p2 point2)
                || (matching p1 point2 && matching p2 point1)
        )
        all


occupiedItem : Level -> Maybe ItemType
occupiedItem level =
    let
        rendered =
            renderPath level.path
    in
        level.items
            |> List.filter
                (\item ->
                    overlapping { x = item.x, y = item.y } rendered
                )
            |> List.head
            |> Maybe.map .kind


renderPath : Path -> List Coords
renderPath (Path start moves) =
    let
        move direction ( { x, y }, rendered ) =
            let
                newPoint =
                    case direction of
                        Left ->
                            { x = x - 1
                            , y = y
                            }

                        Right ->
                            { x = x + 1
                            , y = y
                            }

                        Up ->
                            { x = x
                            , y = y - 1
                            }

                        Down ->
                            { x = x
                            , y = y + 1
                            }
            in
                ( newPoint, newPoint :: rendered )

        points =
            List.reverse <| Tuple.second <| List.foldl move ( start, [] ) moves
    in
        start :: points


move : Direction -> Path -> Path
move direction (Path start remainder) =
    Path start (remainder ++ [ direction ])


resetPath : Path -> Path
resetPath (Path start _) =
    Path start []


resizeGrid : Int -> Int -> Model -> Model
resizeGrid width height model =
    { model
        | grid = Grid.resize width height model.grid
        , width = width
        , height = height
    }


selectable : List a -> List a -> a -> Selectable a
selectable past upcoming current =
    { past = past
    , current = current
    , upcoming = upcoming
    }
