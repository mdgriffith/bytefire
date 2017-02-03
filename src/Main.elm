module Main exposing (..)

{-|
-}

import Html exposing (..)
import Html.Attributes exposing (..)


main : Program Never Int Msg
main =
    Html.program
        { init = 0 ! []
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


type alias Model =
    Int


type Msg
    = WeDontDoAnythingHere


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WeDontDoAnythingHere ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div
        []
        [ text "yo!"
        ]
