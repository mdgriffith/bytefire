module Grid exposing (..)

import Svg exposing (Svg)
import Svg.Attributes exposing (..)


type Grid
    = Grid
        { offset : Coords
        , columns : Int
        , rows : Int
        , rowHeight : Int
        , columnWidth : Int
        }


init : Int -> Int -> Int -> Int -> Grid
init columnWidth rowHeight actualWidth actualHeight =
    Grid
        { offset = ( 0, 0 )
        , columns = actualWidth // columnWidth
        , rows = actualHeight // rowHeight
        , columnWidth = columnWidth
        , rowHeight = rowHeight
        }


resize : Int -> Int -> Grid -> Grid
resize actualWidth actualHeight (Grid grid) =
    Grid
        { grid
            | columns = actualWidth // grid.columnWidth
            , rows = actualHeight // grid.rowHeight
        }


view : Grid -> Svg msg
view (Grid grid) =
    let
        coords =
            List.concatMap
                (\col ->
                    List.map
                        (\row ->
                            ( col, row )
                        )
                        (List.range 0 grid.rows)
                )
                (List.range 0 grid.columns)
    in
        Svg.g []
            (List.map
                (\( x, y ) ->
                    Svg.circle
                        [ cx <| toString (x * grid.columnWidth)
                        , cy <| toString (y * grid.rowHeight)
                        , fill "#ccc"
                        , stroke "rgba(0,0,0,0.0)"
                        , r "2"
                        ]
                        []
                )
                coords
            )


type alias Dimensions =
    ( Int, Int )


type alias Coords =
    ( Int, Int )


type Horizontal
    = Left
    | Right
    | HCenter


type Vertical
    = Top
    | Bottom
    | VCenter


pos : Grid -> ( Int, Int ) -> ( Int, Int )
pos grid ( x, y ) =
    ( x, y )


subgrid : Horizontal -> Vertical -> Dimensions -> Grid -> Grid
subgrid h v ( width, height ) grid =
    grid
