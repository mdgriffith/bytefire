module Main exposing (..)

{-|
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Svg
import Svg.Attributes
import Model exposing (..)
import Selectable exposing (Selectable)
import Window
import Grid exposing (Grid)


main : Program Never Model Msg
main =
    Html.program
        { init = initialModel ! []
        , view = view
        , update = update
        , subscriptions = (\_ -> Window.resizes WindowResize)
        }


type Msg
    = AddInstruction Instruction
    | RemoveInstruction Int Int
    | Select Int
    | Execute
    | Fail
    | NoOp
    | WindowResize
        { width : Int
        , height : Int
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AddInstruction instruction ->
            ( model, Cmd.none )

        RemoveInstruction fnIndex registerIndex ->
            ( model, Cmd.none )

        Select index ->
            ( model, Cmd.none )

        Execute ->
            ( model, Cmd.none )

        Fail ->
            ( model, Cmd.none )

        WindowResize { width, height } ->
            let
                minWidth =
                    if width < 1000 then
                        1000
                    else
                        width

                minHeight =
                    if height < 600 then
                        600
                    else
                        height
            in
                ( resizeGrid minWidth minHeight model
                , Cmd.none
                )


stylesheet =
    """
body {
    background-color: black;
    color: #FAFAFA;
    font-family: courier;
    position:relative;

}
.svg-base {
    width:100%;
    height:100%;
    position:absolute;
    display:block;
}
.basic-square {
    stroke: #CCC;
    background-color: #050505;
    stroke-width:1;
}
.overlay {
    position:absolute;
}


"""


view : Model -> Html Msg
view model =
    div []
        [ node "style" [] [ text stylesheet ]
        , viewGrid model.grid
        , viewLevel model.levels.current
        , div [ class "overlay" ] [ text "bytefire" ]
        ]


viewGrid : Grid -> Html Msg
viewGrid grid =
    Svg.svg
        [ Svg.Attributes.class "svg-base"
        ]
        [ Grid.view grid
        ]


viewLevel : Level -> Html Msg
viewLevel level =
    Svg.svg
        [ Svg.Attributes.class "svg-base"
        ]
        [ viewMap level.map
        , viewRegisters level.registers
        , viewFunctionUI allInstructions
        ]


viewMap : Selectable Location -> Html Msg
viewMap selectableLocations =
    Svg.g []
        (Selectable.mapLocation
            (\pos location ->
                case pos of
                    Selectable.Past ->
                        square location.x location.y

                    Selectable.Current ->
                        square location.x location.y

                    Selectable.Upcoming ->
                        square location.x location.y
            )
            selectableLocations
        )


viewRegisters : Selectable Function -> Html Msg
viewRegisters selectableRegisters =
    Svg.g []
        (Selectable.indexedMapLocation
            (\pos i register ->
                case pos of
                    Selectable.Past ->
                        square 200 (i * 20)

                    Selectable.Current ->
                        square 200 (i * 20)

                    Selectable.Upcoming ->
                        square 200 (i * 20)
            )
            selectableRegisters
        )


viewFunctionUI : List Instruction -> Html Msg
viewFunctionUI fns =
    Svg.g [] []


square : Int -> Int -> Html msg
square x y =
    Svg.rect
        [ Svg.Attributes.width (toString <| 20)
        , Svg.Attributes.height (toString <| 20)
        , Svg.Attributes.x (toString <| x)
        , Svg.Attributes.y (toString <| y)
        , Svg.Attributes.class "basic-square"
        ]
        []
