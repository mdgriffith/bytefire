module Grid exposing (..)

import Svg exposing (Svg)
import Svg.Lazy
import Svg.Attributes exposing (..)
import Color exposing (Color)


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


view : Grid -> (( Int, Int ) -> ( Int, Int ) -> Svg msg) -> Svg msg
view (Grid grid) fn =
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
                    fn ( x, y ) ( x * grid.columnWidth, y * grid.rowHeight )
                )
                coords
            )


rgbColor : Color -> String
rgbColor color =
    let
        { red, green, blue, alpha } =
            Color.toRgb color
    in
        "rgba("
            ++ toString red
            ++ ","
            ++ toString green
            ++ ","
            ++ toString blue
            ++ ","
            ++ toString alpha
            ++ ")"


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


type alias Anchor =
    ( Vertical, Horizontal )


topRight : Anchor
topRight =
    ( Top, Right )


pos : Grid -> ( Int, Int ) -> ( Int, Int )
pos (Grid grid) ( x, y ) =
    ( x * grid.columnWidth, y * grid.rowHeight )


transform : Grid -> Int -> Int -> Svg.Attribute msg
transform grid x y =
    Svg.Attributes.transform <| "translate(" ++ toString (posX grid x) ++ "," ++ toString (posY grid y) ++ ")"


transformFrom : Grid -> Anchor -> Int -> Int -> Svg.Attribute msg
transformFrom (Grid grid) anchor xDelta yDelta =
    let
        ( v, h ) =
            anchor

        y =
            case v of
                Top ->
                    0 + yDelta

                Bottom ->
                    grid.rows - yDelta

                VCenter ->
                    (grid.rows // 2) + yDelta

        x =
            case h of
                Left ->
                    0 + xDelta

                Right ->
                    grid.columns - xDelta

                HCenter ->
                    (grid.columns // 2) + xDelta
    in
        Svg.Attributes.transform <| "translate(" ++ toString (posX (Grid grid) x) ++ "," ++ toString (posY (Grid grid) y) ++ ")"


posX : Grid -> Int -> Int
posX (Grid grid) x =
    x * grid.columnWidth


posY : Grid -> Int -> Int
posY (Grid grid) y =
    y * grid.rowHeight


subgrid : Horizontal -> Vertical -> Dimensions -> Grid -> Grid
subgrid h v ( width, height ) grid =
    grid
