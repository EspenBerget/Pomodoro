module Timer exposing (Timer, display, second, viewTime)

import Browser
import Time exposing (toMinute, toSecond, utc, millisToPosix)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)

-- MODEL
type Timer 
    = Going Int     -- Timer is ticking
    | Paused Int    -- Timer is paused

map : (Int -> Int) -> Timer -> Timer
map f timer =
    case timer of
        Going time ->
            Going (f time)
        _ ->
            timer
    
toggle : Timer -> Timer
toggle timer = 
    case timer of
        Going time ->
            Paused time
        Paused time ->
            Going time

stop : Timer -> Timer
stop timer =
    case timer of
        Going time ->
            Paused time
        _ -> 
            timer

continue : Timer -> Timer 
continue timer =
    case timer of
        Paused time -> 
            Going time
        _ ->
            timer

toPosix : Timer -> Time.Posix
toPosix timer =
    (case timer of
        Going time -> 
            time
        Paused time ->
            time
    )
    |> millisToPosix

type Msg
    = Tick Time.Posix
    | Stop
    | Continue
    | Restart
    | Toggle

posixToString : Time.Posix -> String
posixToString posix =
    (String.padLeft 2 '0' <| String.fromInt <| toMinute utc posix)
    ++ ":" ++
    (String.padLeft 2 '0' <| String.fromInt <| toSecond utc posix)

display : Timer -> String
display timer =
    posixToString <| toPosix timer

-- VIEW

-- NOTE maybe not right to implement this here
--  user will probably want to implement it themself
viewTime : Timer -> Html Msg
viewTime timer =
    h3 [ class "timer" ] [ text <| display timer ]

view : Timer -> Html Msg
view timer =
    div []
        [ viewTime timer
        , button [ onClick Stop ] [ text "Stop" ]
        , button [ onClick Toggle ] [ text "Toggle" ]
        , button [ onClick Continue ] [ text "Continue" ]
        , button [ onClick Restart ] [ text "Restart" ]
        ]

-- UPDATE
update : Msg -> Timer -> ( Timer, Cmd Msg )
update msg timer =
    case msg of
        Tick posix ->
            ( map (\time -> time + 1000) timer, Cmd.none )
        Stop -> 
            ( stop timer, Cmd.none )
        Continue ->
            ( continue timer, Cmd.none )
        Restart ->
            ( Going 0, Cmd.none )
        Toggle ->
            ( toggle timer, Cmd.none )

-- SUB
second : Timer -> Sub Msg
second _ = 
    Time.every 1000 Tick


main : Program () Timer Msg
main = 
    Browser.element
        { init = \_ -> ( Paused 0, Cmd.none )
        , view = view
        , update = update
        , subscriptions = second
        }