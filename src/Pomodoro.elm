module Pomodoro exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Time
import Timer exposing (Timer, isLess)


-- MODEL
type Mode
    = Work       -- Count when work should be done
    | ShortBreak -- A short break in between work sessions
    | LongBreak  -- A longer break between sessions
    | Off        -- timer is off

type alias Model =
    { mode  : Mode
    , round : Int 
    , timer : Timer
    , stopclock : Timer -- Paused timer used to keep track of when to stop
    }

modeToString : Mode -> String
modeToString mode = 
    case mode of 
        Work       -> "Work"
        ShortBreak -> "Short break"
        LongBreak  -> "Long break"
        Off        -> ""

-- VIEW
view : Model -> Html Msg
view model = 
    div []
        [ viewMode model.mode
        , p [] [ text (String.fromInt model.round) ]
        , viewTime model.timer
        , button [ onClick Toggle ] [ text (if Timer.going model.timer then "pause" else "start") ]
        , button [ onClick Reset ] [ text "reset" ]
        ]


viewMode : Mode -> Html Msg
viewMode mode =
    case mode of
        Off -> 
            h3 [] [ text "Press begin"]
        _ ->
            h3 [] [ text (modeToString mode) ]

viewTime  : Timer.Timer -> Html Msg
viewTime timer = 
    h3 [] [ text (Timer.toString timer) ]

-- UPDATE

swap : Model -> Model
swap model = 
    case model.mode of
        Work -> 
            if model.round == 3 then
                { mode = LongBreak
                , round = 0
                , timer = Timer.restart
                , stopclock = longBreakStopclock
                }
            else
                { mode = ShortBreak
                , round = model.round + 1
                , timer = Timer.restart
                , stopclock = shortBreakStopclock
                }
        ShortBreak ->
            { model | mode = Work, timer = Timer.restart, stopclock = workStopclock }
        LongBreak ->
            { model | mode = Work, timer = Timer.restart, stopclock = workStopclock }
        Off -> 
            model

setMode : Mode -> Mode
setMode mode = 
    case mode of
        Off ->
            Work
        _ ->
            mode

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of 
        Tick _ -> 
            if isLess model.timer model.stopclock then
                ( { model | timer = Timer.tick Timer.second model.timer }, Cmd.none )
            else
                (swap model, Cmd.none)
        Reset -> 
            init ()
        Toggle ->
            ( { model | mode = setMode model.mode, timer = Timer.toggle model.timer }, Cmd.none )

type Msg 
    = Tick Time.Posix
    | Reset
    | Toggle

-- SUBS
second : Model -> Sub Msg
second _ =
    Time.every 1000 Tick

-- MAIN
workStopclock : Timer
workStopclock = Timer.fromSecond 10         -- dummy value
shortBreakStopclock : Timer
shortBreakStopclock = Timer.fromSecond 2    -- dummy value
longBreakStopclock : Timer
longBreakStopclock = Timer.fromSecond 5     -- dummy value

init : () -> ( Model, Cmd Msg )
init _ = 
    ({ mode = Off
     , round = 0
     , timer = Timer.reset
     , stopclock = workStopclock
     }
     , Cmd.none
    )

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = second
        }