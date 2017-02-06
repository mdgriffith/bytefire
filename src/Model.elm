module Model exposing (..)

import Selectable exposing (Selectable)
import Grid exposing (Grid)
import Time exposing (Time)


type alias Model =
    { levels : Selectable Level
    , grid : Grid
    , running : Bool
    , time : Time
    }


type alias Level =
    { map : Selectable Location
    , registers : Selectable Function
    }


type alias Location =
    { x : Int
    , y : Int
    , star : Bool
    }


type Instruction
    = Forward
    | Backward
    | Left
    | Right


{-| Problem with making a game about programming is that you get overlapping words :/
-}
type alias Function =
    { instructions : List (Maybe Instruction)
    , name : String
    }


initialModel : Model
initialModel =
    { levels = selectable [] [] levelOne
    , grid = Grid.init 30 30 1000 600
    , running = True
    , time = 0
    }


resizeGrid : Int -> Int -> Model -> Model
resizeGrid width height model =
    { model
        | grid = Grid.resize width height model.grid
    }


levelOne : Level
levelOne =
    { map = selectable [] [] startingLocation
    , registers =
        selectable []
            []
            { instructions = List.repeat 5 Nothing
            , name = "function 1"
            }
        --, mapGrid = Grid.subgrid Grid.HCenter Grid.VCenter (100,100)
        --, registerGrid =
        --    Grid.subgrid Grid.HCenter Grid.Top (80, 20)
    }


startingLocation : Location
startingLocation =
    { x = 20
    , y = 10
    , star = False
    }


selectable : List a -> List a -> a -> Selectable a
selectable past upcoming current =
    { past = past
    , current = current
    , upcoming = upcoming
    }


allInstructions : List Instruction
allInstructions =
    [ Forward
    , Backward
    , Left
    , Right
    ]
