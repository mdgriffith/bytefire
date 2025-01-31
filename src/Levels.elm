module Levels exposing (..)

import Model exposing (..)
import Selectable exposing (Selectable)
import List.Extra
import Combine exposing (..)
import Char
import Grid exposing (Grid(..))


center : Grid -> Level -> Level
center (Grid { columns, rows }) level =
    let
        addPoint { x, y } ( originX, originY, width, height ) =
            case ( originX, originY, width, height ) of
                ( 0, 0, 0, 0 ) ->
                    ( x, y, 0, 0 )

                _ ->
                    let
                        ( newX, dWidth ) =
                            if x < originX then
                                ( x, originX - x )
                            else
                                ( originX, 0 )

                        ( newY, dHeight ) =
                            if y < originY then
                                ( y, originY - y )
                            else
                                ( y, 0 )

                        newWidth =
                            if originX + width < x then
                                width + (x - (originX + width))
                            else
                                width + dWidth

                        newHeight =
                            if originY + height < y then
                                height + (y - (originY + height))
                            else
                                height + dHeight
                    in
                        ( newX, newY, newWidth, newHeight )

        bounds (Seg p1 p2) box =
            box
                |> addPoint p1
                |> addPoint p2

        ( x, y, width, height ) =
            case level.map of
                Map segments ->
                    List.foldl bounds ( 0, 0, 0, 0 ) segments

        dx =
            ((columns - width) // 2) - x

        dy =
            ((rows - height) // 2) - y

        adjust coords =
            { coords
                | x = coords.x + dx
                , y = coords.y + dy
            }

        mapSegment fn (Seg p1 p2) =
            Seg (fn p1) (fn p2)

        newMap (Map segments) =
            Map <| List.map (mapSegment adjust) segments

        newPath (Path fst tail) =
            Path (adjust fst) tail
    in
        { level
            | map = newMap level.map
            , items = List.map adjust level.items
            , path = newPath level.path
        }


levelOne : Level
levelOne =
    { allowed = AllowMove
    , path = Path { x = 5, y = 5 } []
    , map =
        Map
            [ Seg { x = 5, y = 5 } { x = 6, y = 5 }
            , Seg { x = 6, y = 5 } { x = 7, y = 5 }
            , Seg { x = 7, y = 5 } { x = 8, y = 5 }
            ]
    , stack = []
    , items =
        [ { x = 8
          , y = 5
          , kind = Node
          }
        ]
    , conditionalPrepared = Nothing
    , functions =
        Selectable.singleton
            { instructions =
                [ Nothing, Nothing, Nothing ]
            }
    }


levelTwo : Level
levelTwo =
    { allowed = AllowMoveFn
    , path = Path { x = 5, y = 5 } []
    , map =
        Map
            [ Seg { x = 5, y = 5 } { x = 6, y = 5 }
            , Seg { x = 6, y = 5 } { x = 7, y = 5 }
            , Seg { x = 7, y = 5 } { x = 8, y = 5 }
            , Seg { x = 8, y = 5 } { x = 9, y = 5 }
            , Seg { x = 9, y = 5 } { x = 10, y = 5 }
            , Seg { x = 10, y = 5 } { x = 11, y = 5 }
            ]
    , stack = []
    , items =
        [ { x = 11
          , y = 5
          , kind = Node
          }
        ]
    , conditionalPrepared = Nothing
    , functions =
        Selectable.singleton
            { instructions =
                [ Nothing, Nothing, Nothing ]
            }
    }


levelThree : Level
levelThree =
    { allowed = AllowMoveFn
    , path = Path { x = 5, y = 5 } []
    , map =
        Map
            [ Seg { x = 5, y = 5 } { x = 6, y = 5 }
            , Seg { x = 6, y = 5 } { x = 6, y = 6 }
            , Seg { x = 6, y = 6 } { x = 7, y = 6 }
            , Seg { x = 7, y = 6 } { x = 7, y = 7 }
            , Seg { x = 7, y = 7 } { x = 8, y = 7 }
            , Seg { x = 8, y = 7 } { x = 9, y = 7 }
            , Seg { x = 9, y = 7 } { x = 10, y = 7 }
            , Seg { x = 10, y = 7 } { x = 11, y = 7 }
            , Seg { x = 11, y = 7 } { x = 12, y = 7 }
            ]
    , stack = []
    , items =
        [ { x = 12
          , y = 7
          , kind = Node
          }
        ]
    , conditionalPrepared = Nothing
    , functions =
        Selectable.fromList
            [ { instructions =
                    [ Nothing
                    , Nothing
                    , Nothing
                    , Nothing
                    , Nothing
                    ]
              }
            , { instructions =
                    [ Nothing
                    , Nothing
                    , Nothing
                    , Nothing
                    , Nothing
                    ]
              }
            ]
            |> Maybe.withDefault
                (Selectable.singleton
                    { instructions =
                        [ Nothing
                        , Nothing
                        , Nothing
                        , Nothing
                        , Nothing
                        ]
                    }
                )
    }


greenTest : Level
greenTest =
    { allowed = AllowAll
    , path = Path { x = 5, y = 5 } []
    , map =
        Map
            [ Seg { x = 5, y = 5 } { x = 5, y = 6 }
            ]
    , stack = []
    , items =
        [ { x = 5
          , y = 5
          , kind = Square
          }
        , { x = 5
          , y = 6
          , kind = Node
          }
        ]
    , conditionalPrepared = Nothing
    , functions =
        Selectable.singleton
            { instructions =
                [ Nothing ]
            }
    }


test : Level
test =
    { allowed = AllowAll
    , path = Path { x = 5, y = 5 } []
    , map =
        Map
            [ Seg { x = 5, y = 5 } { x = 6, y = 5 }
            , Seg { x = 6, y = 5 } { x = 6, y = 6 }
            , Seg { x = 6, y = 6 } { x = 7, y = 6 }
            , Seg { x = 7, y = 6 } { x = 7, y = 7 }
            , Seg { x = 7, y = 7 } { x = 8, y = 7 }
            , Seg { x = 8, y = 7 } { x = 9, y = 7 }
            , Seg { x = 9, y = 7 } { x = 10, y = 7 }
            , Seg { x = 10, y = 7 } { x = 11, y = 7 }
            , Seg { x = 11, y = 7 } { x = 12, y = 7 }
            ]
    , stack = []
    , items =
        [ { x = 6
          , y = 5
          , kind = Node
          }
        , { x = 6
          , y = 6
          , kind = Square
          }
        , { x = 8
          , y = 7
          , kind = Circle
          }
        , { x = 12
          , y = 7
          , kind = Node
          }
        ]
    , conditionalPrepared = Nothing
    , functions =
        Selectable.fromList
            [ { instructions =
                    [ Nothing
                    , Nothing
                    , Nothing
                    , Nothing
                    , Nothing
                    ]
              }
            , { instructions =
                    [ Nothing
                    , Nothing
                    , Nothing
                    , Nothing
                    , Nothing
                    ]
              }
            ]
            |> Maybe.withDefault
                (Selectable.singleton
                    { instructions =
                        [ Nothing
                        , Nothing
                        , Nothing
                        , Nothing
                        , Nothing
                        ]
                    }
                )
    }


{-| Automatically generate a level based on it's solution.

Given a starting point, move through each instruction, recording the new position

Only a stack of 4 is allowed before the level is "called".

The only part that's not detirministic is how the if conditions are handled.

They don't have to always correspond to a colored square.


-}
auto : List Function -> Level
auto functions =
    let
        allowed =
            AllowAll

        startingPoint =
            { x = 5, y = 5 }

        ( segments, items ) =
            run startingPoint functions
    in
        { allowed = AllowAll
        , path = Path startingPoint []
        , map =
            Map
                segments
        , stack = []
        , items = items
        , conditionalPrepared = Nothing
        , functions =
            Selectable.fromList functions
                |> Maybe.withDefault emptyFns
                |> Selectable.map
                    (\fn ->
                        { fn
                            | instructions = List.map (\_ -> Nothing) fn.instructions
                        }
                    )
        }


decode : String -> Result.Result String (List Function)
decode str =
    case parse functions (unshift str) of
        Result.Ok ( _, _, n ) ->
            Result.Ok n

        Result.Err ( _, stream, ms ) ->
            Result.Err ("parse error: " ++ (toString ms) ++ ", " ++ (toString stream))


shift : String -> String
shift str =
    str
        |> String.toList
        |> List.map (Char.toCode >> (\x -> x + 2) >> Char.fromCode)
        |> String.fromList


unshift : String -> String
unshift str =
    str
        |> String.toList
        |> List.map (Char.toCode >> (\x -> x - 2) >> Char.fromCode)
        |> String.fromList


functions =
    let
        makeFn inst =
            { instructions = List.map Just inst }
    in
        sepBy (string "|") (makeFn <$> (many instructions))


instructions =
    choice
        [ Move Up <$ string "u"
        , Move Down <$ string "d"
        , Move Left <$ string "l"
        , Move Right <$ string "r"
        , Move Left <$ string "l"
        , string "c" $> Call <*> parseCall
        ]


parseCall =
    choice
        [ One <$ string "One"
        , Two <$ string "Two"
        , Three <$ string "Three"
        ]



--instructions =
--    choice
--        [ Move Up <$ string "u"
--        , Move Down <$ string "d"
--        , Move Left <$ string "l"
--        , Move Right <$ string "r"
--        ]


encode : List Function -> String
encode functions =
    functions
        |> List.map fnToString
        |> String.join "|"
        |> shift


fnToString : Function -> String
fnToString fn =
    let
        conditionToString cond =
            case cond of
                Node ->
                    "n"

                Square ->
                    "sq"

                Circle ->
                    "cir"

        toInstructionString inst =
            case inst of
                Move Left ->
                    "l"

                Move Right ->
                    "r"

                Move Up ->
                    "u"

                Move Down ->
                    "d"

                Call fnIndex ->
                    "c" ++ (toString <| realIndex fnIndex)

                If condition instruction ->
                    "if" ++ (conditionToString condition) ++ toInstructionString instruction

                DoNothing ->
                    "-"
    in
        fn.instructions
            |> List.filterMap
                (\mInstruct ->
                    case mInstruct of
                        Nothing ->
                            Nothing

                        Just inst ->
                            Just <| toInstructionString inst
                )
            |> String.concat


emptyFns =
    Selectable.singleton
        { instructions =
            [ Nothing
            ]
        }


run : Coords -> List Function -> ( List LineSegment, List Item )
run coords functions =
    let
        exec =
            execution coords functions

        executed =
            List.foldl stepExecution exec (List.range 0 100)
    in
        ( locationSegments executed.locations
        , locationItems executed.locations
        )


locationItems : List Location -> List Item
locationItems locations =
    let
        withEndStar =
            case List.reverse locations of
                [] ->
                    locations

                last :: tail ->
                    case last.item of
                        Nothing ->
                            List.reverse ({ last | item = Just Node } :: tail)

                        Just item ->
                            -- This could potentially break a puzzle
                            List.reverse ({ last | item = Just Node } :: tail)
    in
        List.filterMap
            (\loc ->
                case loc.item of
                    Nothing ->
                        Nothing

                    Just item ->
                        Just
                            { x = loc.x
                            , y = loc.y
                            , kind = item
                            }
            )
            withEndStar


locationSegments : List Location -> List LineSegment
locationSegments locations =
    let
        toTuple p1 p2 =
            ( p1.x, p1.y, p2.x, p2.y )

        toSegment ( x1, y1, x2, y2 ) =
            Seg
                { x = x1
                , y = y1
                }
                { x = x2
                , y = y2
                }
    in
        List.map2 toTuple locations (List.drop 1 locations)
            |> List.Extra.unique
            |> List.map toSegment


execution : Coords -> List Function -> Execution
execution coord fns =
    { previous = coord
    , functions = Maybe.withDefault emptyFns <| Selectable.fromList fns
    , stack = []
    , instructionIndex = 0
    , locations =
        [ { x = coord.x
          , y = coord.y
          , item = Nothing
          }
        ]
    }


stepExecution : Int -> Execution -> Execution
stepExecution _ execution =
    if finished execution then
        execution
    else
        let
            currentInstruction =
                execution.functions.current.instructions
                    |> List.drop execution.instructionIndex
                    |> List.head
                    |> (\inst ->
                            case inst of
                                Just i ->
                                    i

                                Nothing ->
                                    Nothing
                       )
        in
            case currentInstruction of
                Nothing ->
                    execution

                Just instruction ->
                    case step execution.previous instruction of
                        Loc location ->
                            { execution
                                | locations = execution.locations ++ [ location ]
                                , instructionIndex = execution.instructionIndex + 1
                                , previous =
                                    { x = location.x
                                    , y = location.y
                                    }
                            }

                        GoTo namedIndex ->
                            let
                                index =
                                    case namedIndex of
                                        One ->
                                            0

                                        Two ->
                                            1

                                        Three ->
                                            2
                            in
                                { execution
                                    | functions = Selectable.select index execution.functions
                                    , instructionIndex = 0
                                    , stack =
                                        (StackLevel
                                            (Selectable.currentIndex execution.functions)
                                            (execution.instructionIndex + 1)
                                        )
                                            :: execution.stack
                                }


finished : Execution -> Bool
finished execution =
    ((List.length execution.functions.current.instructions == execution.instructionIndex)
        && (List.length execution.stack == 0)
    )
        || (List.length execution.stack > 5)


type alias Execution =
    { previous : Coords
    , functions : Selectable Function
    , stack : List StackLevel
    , instructionIndex : Int
    , locations : List Location
    }


type alias Location =
    { x : Int
    , y : Int
    , item : Maybe ItemType
    }


type Step
    = Loc Location
    | GoTo FnIndex


step : { a | x : Int, y : Int } -> Instruction -> Step
step { x, y } fn =
    case fn of
        Move Left ->
            Loc
                { x = x - 1
                , y = y
                , item = Nothing
                }

        Move Right ->
            Loc
                { x = x + 1
                , y = y
                , item = Nothing
                }

        Move Up ->
            Loc
                { x = x
                , y = y - 1
                , item = Nothing
                }

        Move Down ->
            Loc
                { x = x
                , y = y + 1
                , item = Nothing
                }

        Call fnIndex ->
            GoTo fnIndex

        If condition instruction ->
            let
                newLocation =
                    step { x = x, y = y } instruction
            in
                case newLocation of
                    Loc newLoc ->
                        Loc
                            { x = newLoc.x
                            , y = newLoc.y
                            , item = Just condition
                            }

                    GoTo _ ->
                        Loc
                            { x = x
                            , y = y
                            , item = Just condition
                            }

        DoNothing ->
            Loc
                { x = x, y = y, item = Nothing }
