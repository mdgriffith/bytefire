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
import Html.Events exposing (..)
import Svg
import Svg.Attributes
import Color
import Keyboard
import Model exposing (..)
import Selectable exposing (Selectable)
import Grid exposing (Grid, rgbColor)
import List.Extra
import Levels


initialModel : Model
initialModel =
    { grid = Grid.init 60 60 1000 600
    , width = 1000
    , height = 600
    , running = True
    , time = 0
    , mode = Playing
    , levels =
        Selectable.singleton <|
            Levels.auto <|
                debugEncode
                    [ { instructions =
                            [ Just (Move Right)
                            , Just (Move Right)
                            , Just (Move Right)
                            , Just (Move Right)
                            , Just (Move Right)
                            ]
                      }
                    ]
        --Selectable.fromList [ Levels.levelOne, Levels.levelTwo, Levels.levelThree ]
        --    |> Maybe.withDefault (Selectable.singleton Levels.levelOne)
    }


debugEncode : List Function -> List Function
debugEncode functions =
    let
        _ =
            Debug.log "encoded" (Levels.encode functions)
    in
        functions


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
                            case model.mode of
                                Paused ->
                                    if code == keyboard.esc then
                                        TogglePause
                                    else
                                        NoOp

                                Playing ->
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
                                        Select (Selectable.nextCycledIndex model.levels.current.functions)
                                    else
                                        NoOp

                                _ ->
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
    | PrepareConditional ItemType
    | Select Int
    | NextLevel
    | Execute
    | ExecuteNextStep
    | TogglePause
    | Tick Time
    | WindowResize
        { width : Int
        , height : Int
        }


type alias Keys =
    { a : Int
    , backspace : Int
    , d : Int
    , down : Int
    , enter : Int
    , esc : Int
    , left : Int
    , one : Int
    , right : Int
    , s : Int
    , tab : Int
    , three : Int
    , two : Int
    , up : Int
    , w : Int
    , space : Int
    }


keyboard : Keys
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


onCurrentLevel : Model -> (Level -> Level) -> Model
onCurrentLevel model fn =
    { model
        | levels =
            Selectable.mapCurrent fn model.levels
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Reboot ->
            ( onCurrentLevel { model | mode = Playing }
                (\level ->
                    { level
                        | functions = Selectable.map resetFunctions level.functions
                        , path = resetPath level.path
                    }
                )
            , Cmd.none
            )

        NextLevel ->
            ( { model | levels = Selectable.next model.levels }
            , Cmd.none
            )

        AddInstruction instruction ->
            ( onCurrentLevel model
                (\level ->
                    { level
                        | conditionalPrepared = Nothing
                        , functions = Selectable.mapCurrent (replaceFirstNothing instruction) level.functions
                    }
                )
            , Cmd.none
            )

        RemoveInstruction ->
            ( onCurrentLevel model
                (\level ->
                    { level
                        | functions =
                            Selectable.mapCurrent removeLatest level.functions
                    }
                )
            , Cmd.none
            )

        PrepareConditional item ->
            ( onCurrentLevel model
                (\level ->
                    { level
                        | conditionalPrepared =
                            if Just item == level.conditionalPrepared then
                                Nothing
                            else
                                Just item
                    }
                )
            , Cmd.none
            )

        Select index ->
            ( onCurrentLevel model
                (\level ->
                    { level | functions = Selectable.select index level.functions }
                )
            , Cmd.none
            )

        Execute ->
            update ExecuteNextStep
                { model
                    | mode = Executing 0 model.time
                    , levels =
                        Selectable.mapCurrent
                            (\level ->
                                { level | functions = Selectable.select 0 level.functions }
                            )
                            model.levels
                }

        ExecuteNextStep ->
            case model.mode of
                Executing i _ ->
                    let
                        maybeInstruction =
                            List.drop i model.levels.current.functions.current.instructions
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
                                if winning model.levels.current.path model.levels.current.items then
                                    ( { model
                                        | mode = Success model.time
                                      }
                                    , Cmd.none
                                    )
                                else if losing model.levels.current.map model.levels.current.path then
                                    ( { model
                                        | mode = Failed model.time OutOfBounds
                                      }
                                    , Cmd.none
                                    )
                                else
                                    case model.levels.current.stack of
                                        [] ->
                                            ( { model
                                                | mode = Failed model.time NoInstructions
                                              }
                                            , Cmd.none
                                            )

                                        (StackLevel fnIndex step) :: remain ->
                                            { model
                                                | mode = Executing step model.time
                                                , levels = Selectable.mapCurrent (\level -> { level | stack = remain }) model.levels
                                            }
                                                |> update (Select fnIndex)
                                                |> andThen ExecuteNextStep

                            Just instruction ->
                                let
                                    ( newLevel, mode ) =
                                        resolveInstruction model.time i instruction model.levels.current
                                in
                                    ( { model
                                        | mode = mode
                                        , levels = Selectable.mapCurrent (\_ -> newLevel) model.levels
                                      }
                                    , Cmd.none
                                    )

                _ ->
                    ( { model
                        | mode = Failed model.time NoInstructions
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

                    Failed failTime _ ->
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

                    Success successTime ->
                        if time - successTime > 1.0 * Time.second then
                            if Selectable.atEnd model.levels then
                                ( { model | mode = GameFinished }, Cmd.none )
                            else
                                update
                                    NextLevel
                                    { model
                                        | time = time
                                        , mode = Playing
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


winning : Path -> List Item -> Bool
winning path items =
    let
        nodes =
            List.filter (\item -> item.kind == Node) items

        rendered =
            renderPath path
    in
        List.all (\node -> overlapping { x = node.x, y = node.y } rendered) nodes


losing : Map -> Path -> Bool
losing (Map segments) path =
    let
        rendered =
            renderPath path

        pathSegments =
            List.map2 (Seg) rendered (List.drop 1 rendered)
    in
        not <| List.all (\seg -> overlappingSegment seg segments) pathSegments


resolveInstruction : Time -> Int -> Instruction -> Level -> ( Level, Mode )
resolveInstruction time i instruction level =
    case instruction of
        DoNothing ->
            ( level, Executing (i + 1) time )

        Move direction ->
            let
                newPath =
                    move direction level.path
            in
                if winning newPath level.items then
                    ( { level
                        | path = newPath
                      }
                    , Success time
                    )
                else if losing level.map newPath then
                    ( { level
                        | path = newPath
                      }
                    , Failed time OutOfBounds
                    )
                else
                    ( { level
                        | path = newPath
                      }
                    , Executing (i + 1) time
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
                    Selectable.currentIndex level.functions
            in
                if List.length level.stack > 5 then
                    ( level
                    , Failed time StackOverflow
                    )
                else
                    ( { level
                        | stack = StackLevel currentRegister (i + 1) :: level.stack
                        , functions =
                            Selectable.select fnIndex level.functions
                      }
                    , Executing 0 time
                    )

        If shape newInstruction ->
            if occupiedItem level == Just shape then
                resolveInstruction time i newInstruction level
            else
                ( level
                , Executing (i + 1) time
                )


stylesheet : String
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
    position:fixed;
    top:0;
    left:0;
    width:100%;
    height:100%;
    background-color: rgba(1,1,1,0);
    font-size:30px;
}
.centered {
   position:absolute;
   width:1000px;
   height:60px;
   left:50%;
   margin-left:-500px;
   top:50%;
   margin-top:-30px;
   text-align:center;
}
.fn-label {
    font-size: 20px;
}
.selection-bracket {
    font-size: 70px;
}
.instruction-control {
    cursor: pointer;
}

"""


view : Model -> Html Msg
view model =
    div [ width model.width, height model.height ]
        [ node "style" [] [ text stylesheet ]
        , Html.Lazy.lazy3 viewGrid model.width model.height model.grid
        , viewLevel model model.levels.current
        , case model.mode of
            Paused ->
                div [ class "overlay" ]
                    [ div [ class "centered", style [ ( "color", rgbColor Color.red ) ] ]
                        [ text "[ Paused ]" ]
                    ]

            Success _ ->
                div [ class "overlay" ]
                    [ div [ class "centered", style [ ( "color", rgbColor Color.green ) ] ]
                        [ text "[ Success ]" ]
                    ]

            GameFinished ->
                div [ class "overlay" ]
                    [ div [ class "centered", style [ ( "color", rgbColor Color.green ) ] ]
                        [ text "[ You have beaten the game.  That's fantastic. ]" ]
                    ]

            Failed _ reason ->
                div [ class "overlay" ]
                    [ div [ class "centered", style [ ( "color", rgbColor Color.red ) ] ]
                        [ text <| "[ Failure, " ++ toString reason ++ "...Rebooting ]" ]
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
                    , Svg.Attributes.fill (rgbColor Color.charcoal)
                    , Svg.Attributes.stroke "rgba(0,0,0,0.0)"
                    , Svg.Attributes.r "3"
                      --, pulseOpacityOffset (toFloat (x + y)) currentTime
                    ]
                    []
        ]


viewPath : Grid -> Time -> List Item -> Path -> Html Msg
viewPath grid currentTime items path =
    let
        nodes =
            List.filter (\i -> i.kind == Node) items

        point color starting location =
            if overlapping { x = location.x, y = location.y } nodes then
                Svg.g []
                    [ drawStar grid ( location.x, location.y ) 15 [ Svg.Attributes.fill (rgbColor Color.black), Svg.Attributes.stroke (rgbColor Color.yellow) ]
                    ]
            else
                Svg.g
                    [ pulseOpacity currentTime ]
                    [ Svg.circle
                        [ Svg.Attributes.cx <| toString (Grid.posX grid location.x)
                        , Svg.Attributes.cy <| toString (Grid.posY grid location.y)
                        , Svg.Attributes.fill (rgbColor color)
                        , Svg.Attributes.stroke "rgba(0,0,0,0.0)"
                        , Svg.Attributes.r <|
                            if starting then
                                toString <| dotSizes.cursor
                            else
                                toString <| dotSizes.item
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
                                toString <| dotSizes.cursor - 2
                            else
                                toString <| dotSizes.item - 2
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


viewMap : Grid -> Map -> Html Msg
viewMap grid (Map segments) =
    let
        renderedSegments =
            List.map viewSegment segments

        viewSegment (Seg p1 p2) =
            Svg.line
                [ Svg.Attributes.x1 <| toString <| Grid.posX grid p1.x
                , Svg.Attributes.y1 <| toString <| Grid.posY grid p1.y
                , Svg.Attributes.x2 <| toString <| Grid.posX grid p2.x
                , Svg.Attributes.y2 <| toString <| Grid.posY grid p2.y
                , Svg.Attributes.strokeDasharray "5, 5"
                , Svg.Attributes.stroke (rgbColor Color.charcoal)
                , Svg.Attributes.strokeWidth "2"
                , Svg.Attributes.fill "rgba(0,0,0,0.0)"
                ]
                []
    in
        Svg.g []
            (List.map viewSegment segments)


viewLevel : Model -> Level -> Html Msg
viewLevel model { path, functions, items, conditionalPrepared, map, allowed } =
    let
        renderedPath =
            renderPath path
    in
        Svg.svg
            [ Svg.Attributes.class "svg-base"
            , width model.width
            , height model.height
            ]
            [ Svg.filter
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
            , Svg.defs []
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
            , viewMap model.grid map
            , viewPath model.grid model.time items path
            , Svg.g [] (List.map (viewItem model.grid renderedPath model.time) items)
            , viewFunctions model.mode functions model.grid model.time
            , let
                instructions =
                    getAllowedInstructions allowed (Selectable.length functions) conditionalPrepared

                instructionCount =
                    List.length instructions
              in
                Svg.g []
                    (List.indexedMap (viewInstructionControl model.grid) instructions)
            ]


viewInstructionControl : Grid -> Int -> Instruction -> Html Msg
viewInstructionControl grid i instruction =
    Svg.g
        [ Grid.transformFrom grid Grid.topRight 1 (i + 1)
        , Svg.Attributes.class "instruction-control"
        , onClick (addInstruction instruction)
        ]
        [ viewInstruction False 0 (Just instruction) ]


addInstruction : Instruction -> Msg
addInstruction instruction =
    case instruction of
        If condition inst ->
            case inst of
                DoNothing ->
                    PrepareConditional condition

                _ ->
                    AddInstruction instruction

        x ->
            AddInstruction x


dotSizes : { cursor : Int, item : Int }
dotSizes =
    { cursor = 8
    , item = 5
    }


shadowDelta : { cursor : Int, item : Int, captured : Int }
shadowDelta =
    { cursor = 3
    , item = 3
    , captured = 4
    }


drawStar : Grid -> ( Int, Int ) -> Float -> List (Svg.Attribute msg) -> Svg.Svg msg
drawStar grid pos size attrs =
    let
        ( xBase, yBase ) =
            Grid.pos grid pos

        outerPoints =
            List.range 0 4
                |> List.map (\x -> ( size, ((toFloat x / 5) * 2 * pi) - pi / 2 ))
                |> List.map fromPolar
                |> List.map (\( x, y ) -> ( x + toFloat xBase, y + toFloat yBase ))

        innerPoints =
            List.range 0 4
                |> List.map (\x -> ( size / 2, (((toFloat x + 0.5) / 5) * 2 * pi) - pi / 2 ))
                |> List.map fromPolar
                |> List.map (\( x, y ) -> ( x + toFloat xBase, y + toFloat yBase ))

        points =
            List.Extra.interweave outerPoints innerPoints

        asPointString coords =
            List.map (\( x, y ) -> toString x ++ "," ++ toString y) coords
                |> String.join " "
    in
        Svg.polygon
            ([ Svg.Attributes.points (asPointString points)
             ]
                ++ attrs
            )
            []


viewItem : Grid -> List Coords -> Time -> Item -> Html Msg
viewItem grid renderedPath currentTime item =
    case item.kind of
        Node ->
            Svg.g []
                [ drawStar grid
                    ( item.x, item.y )
                    12
                    [ Svg.Attributes.fill (rgbColor Color.yellow)
                    , Svg.Attributes.filter "url(#blurMe)"
                    , pulseOpacity currentTime
                    ]
                , drawStar grid ( item.x, item.y ) 10 [ Svg.Attributes.fill (rgbColor Color.yellow) ]
                ]

        Circle ->
            Svg.g []
                [ Svg.circle
                    [ Svg.Attributes.cx <| toString (Grid.posX grid item.x)
                    , Svg.Attributes.cy <| toString (Grid.posY grid item.y)
                    , Svg.Attributes.fill (rgbColor Color.blue)
                    , Svg.Attributes.stroke "rgba(0,0,0,0.0)"
                    , Svg.Attributes.r <| toString <| dotSizes.item + shadowDelta.item
                    , Svg.Attributes.filter "url(#blurMe)"
                    , pulseOpacity currentTime
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

        Square ->
            let
                baseLen =
                    10

                len =
                    toFloat baseLen

                shadowLen =
                    toFloat <| baseLen + shadowDelta.item

                x =
                    toFloat (Grid.posX grid item.x) - (len / 2)

                y =
                    toFloat (Grid.posX grid item.y) - (len / 2)

                shadowX =
                    (toFloat (Grid.posX grid item.x)) - (shadowLen / 2)

                shadowY =
                    (toFloat (Grid.posX grid item.y)) - (shadowLen / 2)
            in
                Svg.g []
                    [ Svg.rect
                        [ Svg.Attributes.x <| toString shadowX
                        , Svg.Attributes.y <| toString shadowY
                        , Svg.Attributes.fill (rgbColor Color.green)
                        , Svg.Attributes.stroke "rgba(0,0,0,0.0)"
                        , Svg.Attributes.width <| toString <| shadowLen
                        , Svg.Attributes.height <| toString <| shadowLen
                        , Svg.Attributes.filter "url(#blurMe)"
                        , pulseOpacity currentTime
                        ]
                        []
                    , Svg.rect
                        [ Svg.Attributes.x <| toString x
                        , Svg.Attributes.y <| toString y
                        , Svg.Attributes.fill (rgbColor Color.green)
                        , Svg.Attributes.stroke "rgba(0,0,0,0.0)"
                        , Svg.Attributes.width <| toString <| len
                        , Svg.Attributes.height <| toString <| len
                        ]
                        []
                    ]


viewFunctions : Mode -> Selectable Function -> Grid -> Time -> Html Msg
viewFunctions mode selectableFunctions grid currentTime =
    let
        label i =
            Svg.text_
                [ Svg.Attributes.fill "white"
                , Svg.Attributes.class "fn-label"
                , Svg.Attributes.x <| toString -75
                , Svg.Attributes.y <| toString 5
                ]
                [ Svg.text <| "F" ++ toString (i + 1) ]
    in
        Svg.g []
            (Selectable.indexedMapLocation
                (\pos i register ->
                    let
                        isSelected =
                            case pos of
                                Selectable.Current ->
                                    True

                                _ ->
                                    False

                        isSelectedInstruction y =
                            isSelected && isFirstNothing y register

                        elements =
                            List.indexedMap (\j inst -> viewInstruction (isSelectedInstruction j) j inst) register.instructions
                                |> (\ls ->
                                        if i == 0 then
                                            viewHint mode register.instructions currentTime :: ls
                                        else
                                            ls
                                   )
                                |> (::) (label i)
                                |> (\ls ->
                                        if isSelected then
                                            let
                                                numInstructions =
                                                    List.length register.instructions

                                                selection =
                                                    Svg.g [] []

                                                --[ Svg.text_
                                                --    [ Svg.Attributes.x <| toString <| (numInstructions * 55) - 40
                                                --    , Svg.Attributes.y <| toString 47
                                                --    , Svg.Attributes.fill (rgbColor Color.yellow)
                                                --    , pulseOpacity currentTime
                                                --    , Svg.Attributes.class "selection-bracket"
                                                --    ]
                                                --    [ Svg.text "]" ]
                                                --, Svg.text_
                                                --    [ Svg.Attributes.x <| toString -55
                                                --    , Svg.Attributes.y <| toString 47
                                                --    , Svg.Attributes.fill (rgbColor Color.yellow)
                                                --    , pulseOpacity currentTime
                                                --    , Svg.Attributes.class "selection-bracket"
                                                --    ]
                                                --    [ Svg.text "[" ]
                                                --]
                                            in
                                                selection :: ls
                                        else
                                            ls
                                   )
                    in
                        Svg.g [ Grid.transform grid 2 (i + 1) ] elements
                )
                selectableFunctions
            )


viewHint : Mode -> List (Maybe Instruction) -> Time -> Html Msg
viewHint mode insts time =
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
                Svg.text_
                    [ Svg.Attributes.x <| toString (i * 55)
                    , Svg.Attributes.y <| toString (35)
                    , Svg.Attributes.fill (rgbColor Color.yellow)
                    , pulseOpacity time
                    ]
                    [ Svg.text "[ Press Enter to Execute ]" ]

            _ ->
                Svg.text ""


viewInstruction : Bool -> Int -> Maybe Instruction -> Html Msg
viewInstruction selected i mInstruction =
    let
        attributes =
            if selected then
                [ Svg.Attributes.cx <| toString (i * 60)
                , Svg.Attributes.cy <| toString (0)
                , Svg.Attributes.fill (rgbColor Color.darkCharcoal)
                , Svg.Attributes.stroke (rgbColor Color.yellow)
                , Svg.Attributes.strokeWidth "3"
                , Svg.Attributes.r "30"
                , Svg.Attributes.strokeDasharray "4, 4"
                ]
            else
                [ Svg.Attributes.cx <| toString (i * 60)
                , Svg.Attributes.cy <| toString (0)
                , Svg.Attributes.fill (rgbColor Color.darkCharcoal)
                , Svg.Attributes.stroke (rgbColor Color.black)
                , Svg.Attributes.strokeWidth "3"
                , Svg.Attributes.r "30"
                ]
    in
        case mInstruction of
            Nothing ->
                Svg.circle attributes []

            Just instruction ->
                let
                    arrow direction =
                        let
                            ( deltaX, deltaY ) =
                                case direction of
                                    Left ->
                                        ( 8, 0 )

                                    Right ->
                                        ( -8, 0 )

                                    Up ->
                                        ( 0, 8 )

                                    Down ->
                                        ( 0, -8 )
                        in
                            Svg.line
                                [ Svg.Attributes.x1 <| toString <| (i * 60) + deltaX
                                , Svg.Attributes.y1 <| toString (0 + deltaY)
                                , Svg.Attributes.x2 <| toString (i * 60)
                                , Svg.Attributes.y2 <| toString (0)
                                , Svg.Attributes.stroke "#fff"
                                , Svg.Attributes.strokeWidth "10"
                                , Svg.Attributes.markerEnd "url(#arrow)"
                                ]
                                []

                    circle color children =
                        Svg.g []
                            ((Svg.circle
                                (attributes ++ [ Svg.Attributes.fill (rgbColor color) ])
                                []
                             )
                                :: children
                            )

                    textNode txt =
                        Svg.text_
                            [ Svg.Attributes.x <| toString <| (i * 60) - 8
                            , Svg.Attributes.y <| toString <| 5
                            , Svg.Attributes.width "60"
                            , Svg.Attributes.height "30"
                            , Svg.Attributes.fill "#fff"
                            ]
                            [ Svg.text txt ]

                    symbol inst =
                        case inst of
                            Move direction ->
                                [ arrow direction ]

                            Call One ->
                                [ textNode "F1" ]

                            Call Two ->
                                [ textNode "F2" ]

                            Call Three ->
                                [ textNode "F3" ]

                            _ ->
                                []
                in
                    case instruction of
                        DoNothing ->
                            circle Color.darkCharcoal []

                        Move direction ->
                            circle Color.darkCharcoal (symbol instruction)

                        Call One ->
                            circle Color.darkCharcoal (symbol instruction)

                        Call Two ->
                            circle Color.darkCharcoal (symbol instruction)

                        Call Three ->
                            circle Color.darkCharcoal (symbol instruction)

                        If shape instruction ->
                            case shape of
                                Node ->
                                    circle Color.yellow (symbol instruction)

                                Square ->
                                    circle Color.green (symbol instruction)

                                Circle ->
                                    circle Color.blue (symbol instruction)


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
