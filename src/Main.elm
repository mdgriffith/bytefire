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
                    , case model.mode of
                        Paused ->
                            Sub.none

                        _ ->
                            AnimationFrame.times Tick
                    , Keyboard.downs
                        (\code ->
                            if code == keyboard.left then
                                AddInstruction <| Move Left
                            else if code == keyboard.right then
                                AddInstruction <| Move Right
                            else if code == keyboard.up then
                                AddInstruction <| Move Up
                            else if code == keyboard.down then
                                AddInstruction <| Move Down
                            else if code == keyboard.esc then
                                TogglePause
                            else if code == keyboard.enter then
                                Execute
                            else if code == keyboard.backspace then
                                RemoveInstruction
                            else if code == keyboard.one then
                                AddInstruction <| Call One
                            else if code == keyboard.two then
                                AddInstruction <| Call Two
                            else if code == keyboard.three then
                                AddInstruction <| Call Three
                            else if code == keyboard.space then
                                Select (Selectable.nextCycledIndex model.registers)
                            else
                                NoOp
                        )
                    ]
            )
        }


type Msg
    = NoOp
    | Reboot
    | AddInstruction Instruction
    | RemoveInstruction
    | Select Int
    | Execute
    | ExecuteNextStep
    | TogglePause
    | Fail
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
    , enter = 13
    , tab = 9
    , backspace = 8
    , one = 49
    , two = 50
    , three = 51
    }


andThen : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
andThen msg ( model, cmd ) =
    let
        ( updated, newCmd ) =
            update msg model
    in
        ( updated, Cmd.batch [ cmd, newCmd ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Reboot ->
            ( { model
                | registers = Selectable.map resetRegisters model.registers
                , path = resetPath model.path
                , mode = Playing
              }
            , Cmd.none
            )

        AddInstruction instruction ->
            let
                oldRegisters =
                    model.registers

                revisedRegisters =
                    { oldRegisters
                        | current = replaceFirstNothing instruction model.registers.current
                    }
            in
                ( { model | registers = revisedRegisters }
                , Cmd.none
                )

        RemoveInstruction ->
            let
                oldRegisters =
                    model.registers

                revisedRegisters =
                    { oldRegisters
                        | current = removeLatest model.registers.current
                    }
            in
                ( { model | registers = revisedRegisters }
                , Cmd.none
                )

        Select index ->
            ( { model | registers = Selectable.select index model.registers }
            , Cmd.none
            )

        Execute ->
            update ExecuteNextStep
                { model
                    | mode = Executing 0 model.time
                }

        Fail ->
            ( model, Cmd.none )

        ExecuteNextStep ->
            case model.mode of
                Executing i _ ->
                    let
                        maybeInstruction =
                            List.drop i model.registers.current.instructions
                                |> List.head
                                |> (\x ->
                                        case x of
                                            Nothing ->
                                                Nothing

                                            Just x ->
                                                x
                                   )
                    in
                        case maybeInstruction of
                            Nothing ->
                                if winning model.path model.items then
                                    ( { model
                                        | mode = Success
                                      }
                                    , Cmd.none
                                    )
                                else
                                    case model.stack of
                                        [] ->
                                            ( { model
                                                | mode = Failed model.time
                                              }
                                            , Cmd.none
                                            )

                                        (StackLevel fnIndex step) :: remain ->
                                            { model
                                                | mode = Executing step model.time
                                                , stack = remain
                                            }
                                                |> update (Select fnIndex)
                                                |> andThen ExecuteNextStep

                            Just instruction ->
                                case instruction of
                                    Move direction ->
                                        let
                                            newPath =
                                                move direction model.path
                                        in
                                            if winning newPath model.items then
                                                ( { model
                                                    | path = newPath
                                                    , mode = Success
                                                  }
                                                , Cmd.none
                                                )
                                            else
                                                ( { model
                                                    | path = newPath
                                                    , mode = Executing (i + 1) model.time
                                                  }
                                                , Cmd.none
                                                )

                                    Call fn ->
                                        let
                                            fnIndex =
                                                case fn of
                                                    One ->
                                                        0

                                                    Two ->
                                                        1

                                                    Three ->
                                                        2

                                            currentRegister =
                                                Selectable.currentIndex model.registers
                                        in
                                            if List.length model.stack > 5 then
                                                ( { model
                                                    | mode = Failed model.time
                                                  }
                                                , Cmd.none
                                                )
                                            else
                                                ( { model
                                                    | mode = Executing 0 model.time
                                                    , stack = StackLevel currentRegister (i + 1) :: model.stack
                                                    , registers =
                                                        Selectable.select fnIndex model.registers
                                                  }
                                                , Cmd.none
                                                )

                _ ->
                    ( { model
                        | mode = Failed model.time
                      }
                    , Cmd.none
                    )

        Tick time ->
            let
                executeNextStep =
                    case model.mode of
                        Executing _ lastStepTime ->
                            time - lastStepTime > 0.1 * Time.second

                        _ ->
                            False
            in
                case model.mode of
                    Executing _ lastStepTime ->
                        if time - lastStepTime > 0.1 * Time.second then
                            update
                                ExecuteNextStep
                                { model
                                    | time = time
                                }
                        else
                            ( { model
                                | time = time
                              }
                            , Cmd.none
                            )

                    Failed failTime ->
                        if time - failTime > 1.0 * Time.second then
                            update
                                Reboot
                                { model
                                    | time = time
                                }
                        else
                            ( { model
                                | time = time
                              }
                            , Cmd.none
                            )

                    _ ->
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
    position:absolute;
    left:0;
    top:0;
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
    background-color: rgba(1,1,1,0);
}
.centered {
   position:absolute;
   width:300px;
   height:60px;
   left:50%;
   margin-left:-150px;
   top:50%;
   margin-top:-30px;
   text-align:center;
}


"""


view : Model -> Html Msg
view model =
    div [ width model.width, height model.height ]
        [ node "style" [] [ text stylesheet ]
        , Html.Lazy.lazy3 viewGrid model.width model.height model.grid
        , viewLevel model
        , case model.mode of
            Paused ->
                div [ class "overlay" ]
                    [ div [ class "centered", style [ ( "color", rgbColor Color.red ) ] ]
                        [ text "[ Paused ]" ]
                    ]

            Success ->
                div [ class "overlay" ]
                    [ div [ class "centered", style [ ( "color", rgbColor Color.green ) ] ]
                        [ text "[ Success ]" ]
                    ]

            Failed _ ->
                div [ class "overlay" ]
                    [ div [ class "centered", style [ ( "color", rgbColor Color.red ) ] ]
                        [ text "[ Failure...Rebooting ]" ]
                    ]

            _ ->
                text ""
        ]


viewGrid : Int -> Int -> Grid -> Html Msg
viewGrid modelWidth modelHeight grid =
    Svg.svg
        [ Svg.Attributes.class "svg-base"
        , width modelWidth
        , height modelHeight
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


viewPath : List Item -> Path -> Time -> Grid -> Html Msg
viewPath items path currentTime grid =
    let
        nodes =
            List.filter (\i -> i.kind == Node) items

        point color starting location =
            if overlapping { x = location.x, y = location.y } nodes then
                Svg.g []
                    [ Svg.circle
                        [ Svg.Attributes.cx <| toString (Grid.posX grid location.x)
                        , Svg.Attributes.cy <| toString (Grid.posY grid location.y)
                        , Svg.Attributes.stroke (rgbColor color)
                        , Svg.Attributes.strokeWidth "2"
                        , Svg.Attributes.fill "black"
                        , Svg.Attributes.r <| toString <| dotSizes.item + shadowDelta.captured
                        ]
                        []
                    ]
            else
                Svg.g [ pulseOpacity currentTime ]
                    [ Svg.circle
                        [ Svg.Attributes.cx <| toString (Grid.posX grid location.x)
                        , Svg.Attributes.cy <| toString (Grid.posY grid location.y)
                        , Svg.Attributes.fill (rgbColor color)
                        , Svg.Attributes.stroke "rgba(0,0,0,0.0)"
                        , Svg.Attributes.r <|
                            if starting then
                                toString <| dotSizes.cursor + 2
                            else
                                toString <| dotSizes.item + 2
                        , Svg.Attributes.filter "url(#blurMe)"
                        ]
                        []
                    , Svg.circle
                        [ Svg.Attributes.cx <| toString (Grid.posX grid location.x)
                        , Svg.Attributes.cy <| toString (Grid.posY grid location.y)
                        , Svg.Attributes.fill (rgbColor color)
                        , Svg.Attributes.stroke "rgba(0,0,0,0.0)"
                        , Svg.Attributes.r <|
                            if starting then
                                toString <| dotSizes.cursor
                            else
                                toString <| dotSizes.item
                        ]
                        []
                    ]

        rendered =
            renderPath path

        total =
            List.length rendered - 1

        points =
            List.indexedMap (\i p -> point Color.yellow (i == total) p) rendered

        asPointString coords =
            List.map (\{ x, y } -> toString (Grid.posX grid x) ++ "," ++ toString (Grid.posY grid y)) coords
                |> String.join " "

        track =
            Svg.polyline
                [ Svg.Attributes.points (asPointString rendered)
                , Svg.Attributes.strokeDasharray "5, 5"
                , Svg.Attributes.stroke (rgbColor Color.yellow)
                , Svg.Attributes.fill "rgba(0,0,0,0.0)"
                , pulseOpacity currentTime
                ]
                []
    in
        Svg.g []
            (track :: points)



--viewLevel : Level -> Grid -> Time -> Html Msg


viewLevel ({ path, time, grid, registers, items, mode } as model) =
    let
        renderedPath =
            renderPath path
    in
        Svg.svg
            [ Svg.Attributes.class "svg-base"
            , width model.width
            , height model.height
            ]
            [ blurs
            , defs
            , viewPath items path time grid
            , Svg.g [] (List.map (viewItem grid renderedPath time) items)
              --, viewMap level.map grid time
            , viewRegisters mode registers grid time
              --, viewFunctionUI allInstructions
            ]


defs =
    Svg.defs []
        [ Svg.marker
            [ Svg.Attributes.id "arrow"
            , Svg.Attributes.markerWidth "10"
            , Svg.Attributes.markerHeight "10"
            , Svg.Attributes.orient "auto"
            , Svg.Attributes.refX "0"
            , Svg.Attributes.refY "1"
            , Svg.Attributes.markerUnits "strokeWidth"
            ]
            [ Svg.path [ Svg.Attributes.d "M0,0 L0,2 L1.5,1 z", Svg.Attributes.fill "#fff" ] []
            ]
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


winning : Path -> List Item -> Bool
winning path items =
    let
        nodes =
            List.filter (\item -> item.kind == Node) items

        rendered =
            renderPath path
    in
        List.all (\node -> overlapping { x = node.x, y = node.y } rendered) nodes


dotSizes =
    { cursor = 8
    , item = 4
    }


shadowDelta =
    { cursor = 3
    , item = 3
    , captured = 4
    }


viewItem : Grid -> List Coords -> Time -> Item -> Html Msg
viewItem grid renderedPath currentTime item =
    case item.kind of
        Node ->
            Svg.g [ pulseOpacity currentTime ]
                [ Svg.circle
                    [ Svg.Attributes.cx <| toString (Grid.posX grid item.x)
                    , Svg.Attributes.cy <| toString (Grid.posY grid item.y)
                    , Svg.Attributes.fill (rgbColor Color.blue)
                    , Svg.Attributes.stroke "rgba(0,0,0,0.0)"
                    , Svg.Attributes.r <| toString <| dotSizes.item + shadowDelta.item
                    , Svg.Attributes.filter "url(#blurMe)"
                    ]
                    []
                , Svg.circle
                    [ Svg.Attributes.cx <| toString (Grid.posX grid item.x)
                    , Svg.Attributes.cy <| toString (Grid.posY grid item.y)
                    , Svg.Attributes.fill (rgbColor Color.blue)
                    , Svg.Attributes.stroke "rgba(0,0,0,0.0)"
                    , Svg.Attributes.r <| toString <| dotSizes.item
                    ]
                    []
                ]


viewRegisters : Mode -> Selectable Function -> Grid -> Time -> Html Msg
viewRegisters mode selectableRegisters grid currentTime =
    Svg.g []
        (Selectable.indexedMapLocation
            (\pos i register ->
                case pos of
                    Selectable.Past ->
                        Svg.g
                            [ Grid.transform grid 2 (i + 1)
                            ]
                            (List.indexedMap viewInstruction register.instructions)

                    Selectable.Current ->
                        Svg.g
                            [ Grid.transform grid 2 (i + 1)
                            ]
                            (viewEnterToExecute mode register.instructions currentTime :: List.indexedMap viewInstruction register.instructions)

                    Selectable.Upcoming ->
                        Svg.g
                            [ Grid.transform grid 2 (i + 1)
                            ]
                            (List.indexedMap viewInstruction register.instructions)
            )
            selectableRegisters
        )


viewEnterToExecute : Mode -> List (Maybe Instruction) -> Time -> Html Msg
viewEnterToExecute mode insts time =
    let
        i =
            List.length insts
    in
        case mode of
            Executing _ _ ->
                Svg.text_
                    [ Svg.Attributes.x <| toString (i * 55)
                    , Svg.Attributes.y <| toString (35)
                    , Svg.Attributes.fill (rgbColor Color.red)
                    , pulseOpacity time
                    ]
                    [ Svg.text "[ Executing... ]" ]

            Playing ->
                if List.all (\x -> x /= Nothing) insts then
                    Svg.text_
                        [ Svg.Attributes.x <| toString (i * 55)
                        , Svg.Attributes.y <| toString (35)
                        , Svg.Attributes.fill (rgbColor Color.yellow)
                        , pulseOpacity time
                        ]
                        [ Svg.text "[ Press Enter to Execute ]" ]
                else
                    Svg.g []
                        [ Svg.text_
                            [ Svg.Attributes.x <| toString <| (i * 55) - 40
                            , Svg.Attributes.y <| toString 45
                            , Svg.Attributes.fill (rgbColor Color.yellow)
                            , pulseOpacity time
                            , Html.Attributes.style [ ( "font-size", "70" ) ]
                            ]
                            [ Svg.text "]" ]
                        , Svg.text_
                            [ Svg.Attributes.x <| toString -55
                            , Svg.Attributes.y <| toString 45
                            , Svg.Attributes.fill (rgbColor Color.yellow)
                            , pulseOpacity time
                            , Html.Attributes.style [ ( "font-size", "70" ) ]
                            ]
                            [ Svg.text "[" ]
                        ]

            _ ->
                Svg.text ""


viewInstruction : Int -> Maybe Instruction -> Html Msg
viewInstruction i mInstruction =
    case mInstruction of
        Nothing ->
            Svg.circle
                [ Svg.Attributes.cx <| toString (i * 55)
                , Svg.Attributes.cy <| toString (30)
                , Svg.Attributes.fill (rgbColor Color.darkCharcoal)
                , Svg.Attributes.stroke (rgbColor Color.black)
                , Svg.Attributes.strokeWidth "5"
                , Svg.Attributes.r "30"
                ]
                []

        Just instruction ->
            let
                node deltaX deltaY =
                    Svg.g []
                        [ Svg.circle
                            [ Svg.Attributes.cx <| toString (i * 55)
                            , Svg.Attributes.cy <| toString (30)
                            , Svg.Attributes.fill (rgbColor Color.darkCharcoal)
                            , Svg.Attributes.stroke (rgbColor Color.black)
                            , Svg.Attributes.strokeWidth "5"
                            , Svg.Attributes.r "30"
                            ]
                            []
                        , Svg.line
                            [ Svg.Attributes.x1 <| toString <| (i * 55) + deltaX
                            , Svg.Attributes.y1 <| toString (30 + deltaY)
                            , Svg.Attributes.x2 <| toString (i * 55)
                            , Svg.Attributes.y2 <| toString (30)
                            , Svg.Attributes.stroke "#fff"
                            , Svg.Attributes.strokeWidth "10"
                            , Svg.Attributes.markerEnd "url(#arrow)"
                            ]
                            []
                        ]

                textNode txt =
                    Svg.g []
                        [ Svg.circle
                            [ Svg.Attributes.cx <| toString (i * 55)
                            , Svg.Attributes.cy <| toString (30)
                            , Svg.Attributes.fill (rgbColor Color.darkCharcoal)
                            , Svg.Attributes.stroke (rgbColor Color.black)
                            , Svg.Attributes.strokeWidth "5"
                            , Svg.Attributes.r "30"
                            ]
                            []
                        , Svg.text_
                            [ Svg.Attributes.x <| toString <| (i * 55) - 8
                            , Svg.Attributes.y <| toString <| (30) + 5
                            , Svg.Attributes.width "55"
                            , Svg.Attributes.height "30"
                            , Svg.Attributes.fill "#fff"
                            ]
                            [ Svg.text txt ]
                        ]
            in
                case instruction of
                    Move Left ->
                        node 8 0

                    Move Right ->
                        node (-8) 0

                    Move Up ->
                        node 0 8

                    Move Down ->
                        node 0 (-8)

                    Call One ->
                        textNode "F1"

                    Call Two ->
                        textNode "F2"

                    Call Three ->
                        textNode "F3"


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
