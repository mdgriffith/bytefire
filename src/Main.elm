module Main exposing (..)

{-|
-}

import Window
import Time exposing (Time)
import Task
import AnimationFrame
import Html exposing (..)
import Html.Lazy
import Html.Attributes exposing (..)
import Svg
import Svg.Attributes
import Color
import Keyboard
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
                    , if model.mode == Playing then
                        AnimationFrame.times Tick
                      else
                        Sub.none
                    , Keyboard.downs
                        (\code ->
                            if code == keyboard.left then
                                Move Left
                            else if code == keyboard.right then
                                Move Right
                            else if code == keyboard.up then
                                Move Up
                            else if code == keyboard.down then
                                Move Down
                            else if code == keyboard.esc then
                                TogglePause
                            else
                                NoOp
                        )
                    ]
            )
        }


type Msg
    = AddInstruction Instruction
    | RemoveInstruction Int Int
    | Select Int
    | Execute
    | Move Direction
    | TogglePause
    | Fail
    | NoOp
    | Tick Time
    | WindowResize
        { width : Int
        , height : Int
        }


keyboard =
    { left = 37
    , right = 39
    , up = 38
    , down = 40
    , a = 65
    , d = 68
    , w = 87
    , s = 83
    , space = 32
    , esc = 27
    , enter = 27
    , tab = 9
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

        Move direction ->
            ( { model
                | path = move direction model.path
              }
            , Cmd.none
            )

        Tick time ->
            ( { model
                | time = time
              }
            , Cmd.none
            )

        TogglePause ->
            ( { model
                | mode =
                    if model.mode == Paused then
                        Playing
                    else
                        Paused
              }
            , Cmd.none
            )

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
@import url("https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css");
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
    top:0;
    left:0;
    width:100%;
    height:100%;
    background-color: rgba(1,1,1,0.5);
}
.centered {
   position:absolute;
   width:300px;
   height:60px;
   left:50%;
   margin-left:-150px;
   top:50%;
   margin-top:-30px;
}


"""


view : Model -> Html Msg
view model =
    div []
        [ node "style" [] [ text stylesheet ]
        , Html.Lazy.lazy viewGrid model.grid
        , viewLevel model
        , case model.mode of
            Paused ->
                div [ class "overlay" ]
                    [ div [ class "centered" ]
                        [ text "Paused" ]
                    ]

            _ ->
                text ""
        ]


viewGrid : Grid -> Html Msg
viewGrid grid =
    Svg.svg
        [ Svg.Attributes.class "svg-base"
        ]
        [ Grid.view grid <|
            \( col, row ) ( x, y ) ->
                Svg.circle
                    [ Svg.Attributes.cx <| toString x
                    , Svg.Attributes.cy <| toString y
                    , Svg.Attributes.fill (rgbColor Color.darkCharcoal)
                    , Svg.Attributes.stroke "rgba(0,0,0,0.0)"
                    , Svg.Attributes.r "2"
                      --, pulseOpacityOffset (toFloat (x + y)) currentTime
                    ]
                    []
        ]


viewPath : Path -> Time -> Grid -> Html Msg
viewPath (Path start moves) currentTime grid =
    let
        point color location =
            Svg.g [ pulseOpacity currentTime ]
                [ Svg.circle
                    [ Svg.Attributes.cx <| toString (Grid.posX grid location.x)
                    , Svg.Attributes.cy <| toString (Grid.posY grid location.y)
                    , Svg.Attributes.fill (rgbColor color)
                    , Svg.Attributes.stroke "rgba(0,0,0,0.0)"
                    , Svg.Attributes.r "5"
                    , Svg.Attributes.filter "url(#blurMe)"
                    ]
                    []
                , Svg.circle
                    [ Svg.Attributes.cx <| toString (Grid.posX grid location.x)
                    , Svg.Attributes.cy <| toString (Grid.posY grid location.y)
                    , Svg.Attributes.fill (rgbColor color)
                    , Svg.Attributes.stroke "rgba(0,0,0,0.0)"
                    , Svg.Attributes.r "3"
                    ]
                    []
                ]

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
                ( newPoint, point Color.blue newPoint :: rendered )

        remainingPoints =
            Tuple.second <| List.foldl move ( start, [] ) moves
    in
        Svg.g []
            (point Color.yellow start :: remainingPoints)



--viewLevel : Level -> Grid -> Time -> Html Msg


viewLevel { path, time, grid, registers, items } =
    let
        renderedPath =
            renderPath path
    in
        Svg.svg
            [ Svg.Attributes.class "svg-base"
            ]
            [ blurs
            , Svg.g [] (List.map (viewItem grid renderedPath time) items)
              --, viewMap level.map grid time
            , viewRegisters registers grid
              --, viewFunctionUI allInstructions
            , viewPath path time grid
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


viewItem : Grid -> List Coords -> Time -> Item -> Html Msg
viewItem grid renderedPath currentTime item =
    case item.kind of
        Node ->
            if overlapping { x = item.x, y = item.y } renderedPath then
                Svg.g [ pulseOpacity currentTime ]
                    [ Svg.circle
                        [ Svg.Attributes.cx <| toString (Grid.posX grid item.x)
                        , Svg.Attributes.cy <| toString (Grid.posY grid item.y)
                        , Svg.Attributes.fill (rgbColor Color.black)
                        , Svg.Attributes.stroke (rgbColor Color.yellow)
                        , Svg.Attributes.r "8"
                        , Svg.Attributes.filter "url(#blurMe)"
                        ]
                        []
                    , Svg.circle
                        [ Svg.Attributes.cx <| toString (Grid.posX grid item.x)
                        , Svg.Attributes.cy <| toString (Grid.posY grid item.y)
                        , Svg.Attributes.fill (rgbColor Color.black)
                        , Svg.Attributes.stroke (rgbColor Color.black)
                        , Svg.Attributes.r "5"
                        ]
                        []
                    ]
            else
                Svg.g [ pulseOpacity currentTime ]
                    [ Svg.circle
                        [ Svg.Attributes.cx <| toString (Grid.posX grid item.x)
                        , Svg.Attributes.cy <| toString (Grid.posY grid item.y)
                        , Svg.Attributes.fill (rgbColor Color.green)
                        , Svg.Attributes.stroke "rgba(0,0,0,0.0)"
                        , Svg.Attributes.r "5"
                        , Svg.Attributes.filter "url(#blurMe)"
                        ]
                        []
                    , Svg.circle
                        [ Svg.Attributes.cx <| toString (Grid.posX grid item.x)
                        , Svg.Attributes.cy <| toString (Grid.posY grid item.y)
                        , Svg.Attributes.fill (rgbColor Color.green)
                        , Svg.Attributes.stroke "rgba(0,0,0,0.0)"
                        , Svg.Attributes.r "3"
                        ]
                        []
                    ]


viewRegisters : Selectable Function -> Grid -> Html Msg
viewRegisters selectableRegisters grid =
    Svg.g []
        (Selectable.indexedMapLocation
            (\pos i register ->
                case pos of
                    Selectable.Past ->
                        square (Grid.posY grid 3) (Grid.posY grid 20)

                    Selectable.Current ->
                        Svg.g
                            [ Grid.transform grid 3 10
                            ]
                            (List.indexedMap viewInstruction register.instructions)

                    Selectable.Upcoming ->
                        square (Grid.posY grid 3) (Grid.posY grid 20)
            )
            selectableRegisters
        )


viewInstruction : Int -> Maybe Instruction -> Html Msg
viewInstruction i mInstruction =
    case mInstruction of
        Nothing ->
            Svg.circle
                [ Svg.Attributes.cx <| toString (i * 50)
                , Svg.Attributes.cy <| toString (30)
                , Svg.Attributes.fill (rgbColor Color.darkCharcoal)
                , Svg.Attributes.stroke (rgbColor Color.black)
                , Svg.Attributes.strokeWidth "5"
                , Svg.Attributes.r "30"
                ]
                []

        Just instruction ->
            case instruction of
                Model.Move Left ->
                    Svg.g []
                        [ Svg.circle
                            [ Svg.Attributes.cx <| toString (i * 50)
                            , Svg.Attributes.cy <| toString (30)
                            , Svg.Attributes.fill (rgbColor Color.darkCharcoal)
                            , Svg.Attributes.stroke (rgbColor Color.black)
                            , Svg.Attributes.strokeWidth "5"
                            , Svg.Attributes.r "30"
                            ]
                            []
                        , Svg.text_
                            [ Svg.Attributes.stroke (rgbColor Color.white)
                            , Svg.Attributes.x <| toString (i * 50)
                            , Svg.Attributes.y <| toString (30)
                            ]
                            [ text "left" ]
                        ]

                Model.Move Right ->
                    Svg.g []
                        [ Svg.circle
                            [ Svg.Attributes.cx <| toString (i * 50)
                            , Svg.Attributes.cy <| toString (30)
                            , Svg.Attributes.fill (rgbColor Color.darkCharcoal)
                            , Svg.Attributes.stroke (rgbColor Color.black)
                            , Svg.Attributes.strokeWidth "5"
                            , Svg.Attributes.r "30"
                            ]
                            []
                        , Svg.text_
                            [ Svg.Attributes.stroke (rgbColor Color.white)
                            , Svg.Attributes.x <| toString (i * 50)
                            , Svg.Attributes.y <| toString (30)
                            ]
                            [ text "right" ]
                        ]

                Model.Move Up ->
                    Svg.g []
                        [ Svg.circle
                            [ Svg.Attributes.cx <| toString (i * 50)
                            , Svg.Attributes.cy <| toString (30)
                            , Svg.Attributes.fill (rgbColor Color.darkCharcoal)
                            , Svg.Attributes.stroke (rgbColor Color.black)
                            , Svg.Attributes.strokeWidth "5"
                            , Svg.Attributes.r "30"
                            ]
                            []
                        , Svg.text_
                            [ Svg.Attributes.stroke (rgbColor Color.white)
                            , Svg.Attributes.x <| toString (i * 50)
                            , Svg.Attributes.y <| toString (30)
                            ]
                            [ text "up" ]
                        ]

                Model.Move Down ->
                    Svg.g []
                        [ Svg.circle
                            [ Svg.Attributes.cx <| toString (i * 30)
                            , Svg.Attributes.cy <| toString (30)
                            , Svg.Attributes.fill (rgbColor Color.darkCharcoal)
                            , Svg.Attributes.stroke (rgbColor Color.black)
                            , Svg.Attributes.strokeWidth "5"
                            , Svg.Attributes.r "30"
                            ]
                            []
                        , Svg.text_
                            [ Svg.Attributes.stroke (rgbColor Color.white)
                            , Svg.Attributes.x <| toString (i * 50)
                            , Svg.Attributes.y <| toString (30)
                            ]
                            [ text "down" ]
                        ]


viewFunctionUI : List Instruction -> Html Msg
viewFunctionUI fns =
    Svg.g [] []


pulseOpacityOffset : Float -> Time -> Html.Attribute msg
pulseOpacityOffset offset currentTime =
    Svg.Attributes.opacity <| toString <| pulse (currentTime + offset) (2 * Time.second) 0.5 1


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
