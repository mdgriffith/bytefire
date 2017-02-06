module Main exposing (..)

{-|
-}

import Window
import Time exposing (Time)
import Task
import AnimationFrame
import Html exposing (..)
import Html.Attributes exposing (..)
import Svg
import Svg.Attributes
import Color
import Model exposing (..)
import Selectable exposing (Selectable)
import Grid exposing (Grid, rgbColor)


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Task.perform WindowResize Window.size )
        , view = view
        , update = update
        , subscriptions =
            (\model ->
                Sub.batch
                    [ Window.resizes WindowResize
                    , if model.running then
                        AnimationFrame.times Tick
                      else
                        Sub.none
                    ]
            )
        }


type Msg
    = AddInstruction Instruction
    | RemoveInstruction Int Int
    | Select Int
    | Execute
    | Fail
    | NoOp
    | Tick Time
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

        Tick time ->
            ( { model | time = time }, Cmd.none )

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
        , viewLevel model.levels.current model.grid model.time
        , div [ class "overlay" ] [ text "bytefire" ]
        ]


viewGrid : Grid -> Html Msg
viewGrid grid =
    Svg.svg
        [ Svg.Attributes.class "svg-base"
        ]
        [ Grid.view grid Color.darkCharcoal
        ]


viewLevel : Level -> Grid -> Time -> Html Msg
viewLevel level grid time =
    Svg.svg
        [ Svg.Attributes.class "svg-base"
        ]
        [ blurs
        , viewMap level.map grid time
        , viewRegisters level.registers
        , viewFunctionUI allInstructions
        ]


blurs =
    Svg.filter
        [ Svg.Attributes.id "blurMe"
        , Svg.Attributes.x "-50%"
        , Svg.Attributes.y "-50%"
        , Svg.Attributes.width "200%"
        , Svg.Attributes.height "200%"
        ]
        [ Svg.feGaussianBlur
            [ Svg.Attributes.in_ "SourceGraphic"
            , Svg.Attributes.stdDeviation "3.0"
            , Svg.Attributes.result "coloredBlur"
            ]
            []
        , Svg.feMerge []
            [ Svg.feMergeNode [ Svg.Attributes.in_ "coloredBlur" ] []
              --, Svg.feMergeNode [ Svg.Attributes.in_ "SourceGraphic" ] []
            ]
        ]


viewMap : Selectable Location -> Grid -> Time -> Html Msg
viewMap selectableLocations grid currentTime =
    Svg.g []
        (Selectable.mapLocation
            (\pos location ->
                case pos of
                    Selectable.Past ->
                        Svg.g [ pulseOpacity currentTime ]
                            [ Svg.circle
                                [ Svg.Attributes.cx <| toString (Grid.posX grid location.x)
                                , Svg.Attributes.cy <| toString (Grid.posY grid location.y)
                                , Svg.Attributes.fill (rgbColor Color.green)
                                , Svg.Attributes.stroke "rgba(0,0,0,0.0)"
                                , Svg.Attributes.r "5"
                                , Svg.Attributes.filter "url(#blurMe)"
                                ]
                                []
                            , Svg.circle
                                [ Svg.Attributes.cx <| toString (Grid.posX grid location.x)
                                , Svg.Attributes.cy <| toString (Grid.posY grid location.y)
                                , Svg.Attributes.fill (rgbColor Color.green)
                                , Svg.Attributes.stroke "rgba(0,0,0,0.0)"
                                , Svg.Attributes.r "3"
                                ]
                                []
                            ]

                    Selectable.Current ->
                        Svg.g [ pulseOpacity currentTime ]
                            [ Svg.circle
                                [ Svg.Attributes.cx <| toString (Grid.posX grid location.x)
                                , Svg.Attributes.cy <| toString (Grid.posY grid location.y)
                                , Svg.Attributes.fill (rgbColor Color.yellow)
                                , Svg.Attributes.stroke "rgba(0,0,0,0.0)"
                                , Svg.Attributes.r "5"
                                , Svg.Attributes.filter "url(#blurMe)"
                                ]
                                []
                            , Svg.circle
                                [ Svg.Attributes.cx <| toString (Grid.posX grid location.x)
                                , Svg.Attributes.cy <| toString (Grid.posY grid location.y)
                                , Svg.Attributes.fill (rgbColor Color.yellow)
                                , Svg.Attributes.stroke "rgba(0,0,0,0.0)"
                                , Svg.Attributes.r "3"
                                ]
                                []
                            ]

                    Selectable.Upcoming ->
                        Svg.g [ pulseOpacity currentTime ]
                            [ Svg.circle
                                [ Svg.Attributes.cx <| toString (Grid.posX grid location.x)
                                , Svg.Attributes.cy <| toString (Grid.posY grid location.y)
                                , Svg.Attributes.fill (rgbColor Color.darkCharcoal)
                                , Svg.Attributes.stroke "rgba(0,0,0,0.0)"
                                , Svg.Attributes.r "5"
                                , Svg.Attributes.filter "url(#blurMe)"
                                ]
                                []
                            , Svg.circle
                                [ Svg.Attributes.cx <| toString (Grid.posX grid location.x)
                                , Svg.Attributes.cy <| toString (Grid.posY grid location.y)
                                , Svg.Attributes.fill (rgbColor Color.darkCharcoal)
                                , Svg.Attributes.stroke "rgba(0,0,0,0.0)"
                                , Svg.Attributes.r "3"
                                ]
                                []
                            ]
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


pulseOpacity : Time -> Html.Attribute msg
pulseOpacity currentTime =
    Svg.Attributes.opacity <| toString <| pulse currentTime (1 * Time.second) 0.5 1


pulse : Time -> Time -> Float -> Float -> Float
pulse currentTime period low high =
    let
        normalized =
            (currentTime / period) * 2 * pi

        base =
            (sin normalized + 1.0) / 2
    in
        (base * (high - low)) + low


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
