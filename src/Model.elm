module Model exposing (..)

import Selectable exposing (Selectable)
import Grid exposing (Grid)
import Time exposing (Time)


type alias Model =
    { path : Path
    , items : List Item
    , registers : Selectable Function
    , grid : Grid
    , running : Bool
    , time : Time
    , mode : Mode
    }


type Mode
    = Playing
    | Executing Int Time
    | Paused
    | Success
    | Failed Time


type ItemType
    = Node


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


type Instruction
    = Move Direction
    | Call FnIndex


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


resetRegisters : Function -> Function
resetRegisters fn =
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


overlapping : Coords -> List { a | x : Int, y : Int } -> Bool
overlapping coord all =
    List.any (\point -> point.x == coord.x && point.y == coord.y) all


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


initialModel : Model
initialModel =
    { mode = Playing
    , path = Path { x = 5, y = 5 } []
    , items =
        [ { x = 6
          , y = 5
          , kind = Node
          }
        , { x = 6
          , y = 6
          , kind = Node
          }
        , { x = 8
          , y = 7
          , kind = Node
          }
        ]
    , grid = Grid.init 60 60 1000 600
    , running = True
    , time = 0
    , registers =
        selectable []
            [ { instructions =
                    [ Nothing
                    , Nothing
                    , Nothing
                    , Nothing
                    , Nothing
                    ]
              }
            ]
            { instructions =
                [ Nothing
                , Nothing
                , Nothing
                , Nothing
                , Nothing
                ]
            }
    }


resizeGrid : Int -> Int -> Model -> Model
resizeGrid width height model =
    { model
        | grid = Grid.resize width height model.grid
    }



--levels = selectable [] [] levelOne
--levelOne : Level
--levelOne =
--    { map = selectable [] [] startingLocation
--    , registers =
--        selectable []
--            []
--            { instructions =
--                [ Just <| Move Left
--                , Just <| Move Up
--                , Just <| Move Right
--                , Just <| Move Up
--                , Nothing
--                ]
--            }
--    }
--startingLocation : Location
--startingLocation =
--{ x = 20
--, y = 10
--, star = False
--}


selectable : List a -> List a -> a -> Selectable a
selectable past upcoming current =
    { past = past
    , current = current
    , upcoming = upcoming
    }


allInstructions : List Instruction
allInstructions =
    []
