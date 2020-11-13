module Pomodoro exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)

-- MODEL
type alias Model = String
-- VIEW
view  : Model -> Html Msg
view dummy = 
    div []
        [ h3 [] [ text dummy ]
        , button [ onClick Stop ] [ text "Stop" ]
        , button [ onClick Start ] [ text "Start" ]
        ]

-- UPDATE
type Msg 
    = Stop
    | Start

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of 
        Stop -> 
            ( "stop", Cmd.none )
        Start -> 
            ( "start", Cmd.none )

-- MAIN
init : () -> ( Model, Cmd Msg )
init _ =  ("No value", Cmd.none )

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }