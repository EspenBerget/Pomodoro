module Pomodoro exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Time exposing (utc, toSecond, toMinute)
import Task

-- MODEL
type alias Model = Maybe Int

timeToString : Time.Posix -> String
timeToString time = 
    (String.padLeft 2 '0' <| String.fromInt <| Time.toMinute utc time) 
    ++ ":" ++
    (String.padLeft 2 '0' <| String.fromInt <| Time.toSecond utc time)


-- VIEW
view : Model -> Html Msg
view model = 
    div []
        [ viewTime model
        , button [ onClick Stop ] [ text "Stop" ]
        , button [ onClick Start ] [ text "Start" ]
        ]

viewTime  : Model -> Html Msg
viewTime model = 
    case model of
        Just time -> 
            h3 [] [ text (timeToString <| Time.millisToPosix (time*1000)) ]
        Nothing -> 
            h3 [] [ text "00:00" ]

-- UPDATE
type Msg 
    = Tick Time.Posix
    | Start
    | Stop

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of 
        Tick time -> 
            ( Maybe.map (\x -> x+1) model, Cmd.none )
        Stop ->
            ( Nothing, Cmd.none )
        Start ->
            ( Just 0, Cmd.none ) 


-- MAIN
init : () -> ( Model, Cmd Msg )
init _ =  ( Nothing, Cmd.none )

tick : Model -> Sub Msg
tick model =
    Time.every 1000 Tick

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = tick
        }