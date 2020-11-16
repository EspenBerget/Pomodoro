module Pomodoro exposing (main)

import Browser
import Time
import Timer exposing (Timer, isLess)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Browser.Navigation exposing (back)

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
view : Model -> Browser.Document Msg
view model =
    { title = "Pomodoro"
    , body =
        [ layout [behindContent background, clip] (viewMain model) ]
    }

background : Element Msg
background = 
    image [width fill]
          { src = "images/aaron-burden-study.jpg"
          , description = "Background image representing a study environment"
          }

viewMain : Model -> Element Msg
viewMain model = 
    column [ spacing 30
           , padding 50
           , centerX
           , centerY
           , Border.rounded 5
           , Background.color (rgba 0.6 0.1 0.2 0.8)
           , Font.family [ Font.typeface "Helvetica" ]
           , Font.color (rgb255 220 220 220)
           ]
           [ viewMode model.mode
           , el [ Font.bold ] ( text <| "Round " ++ (String.fromInt model.round) )
           , viewTime model.timer
           , row [ spacing 20 ]
                [ btn { onPress = Just Toggle
                  , label = text (if Timer.going model.timer then "pause" else "start")
                  }
                  , btn { onPress = Just Reset
                        , label = text "reset" 
                        }
                ]
           ]

btn : { onPress : Maybe Msg, label : Element Msg} -> Element Msg
btn =
    button
        [ Background.color (rgb255 0 140 186)
        , Border.rounded 5
        , Font.color (rgb255 25 25 25) 
        , width (minimum 75 fill)
        , height (minimum 30 fill)
        , Font.center
        , mouseOver [ Font.color (rgb 220 220 220) ]
        , focused [] -- Noop when focused
        ] 

viewMode : Mode -> Element Msg
viewMode mode =
    (case mode of
        Off -> 
            (text "Press begin")
        _ ->
            (text (modeToString mode)))
    |> el [ Font.size 25
          , Font.family 
                [ Font.typeface "Candara"
                , Font.sansSerif
                ]
          ]

viewTime  : Timer.Timer -> Element Msg
viewTime timer = 
    el [] (text (Timer.toString timer))

-- UPDATE

swap : Model -> Model
swap model = 
    case model.mode of
        Work -> 
            if model.round == 4 then
                { model 
                | mode = LongBreak
                , timer = Timer.restart
                , stopclock = longBreakStopclock
                }
            else
                { model
                | mode = ShortBreak
                , timer = Timer.restart
                , stopclock = shortBreakStopclock
                }
        ShortBreak ->
            { mode = Work
            , round = model.round + 1
            , timer = Timer.restart
            , stopclock = workStopclock
            }
        LongBreak ->
            { mode = Work
            , round = 1
            , timer = Timer.restart
            , stopclock = workStopclock
            }
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
     , round = 1
     , timer = Timer.reset
     , stopclock = workStopclock
     }
     , Cmd.none
    )

main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = second
        }