module Pomodoro exposing (main)

import Browser
import Time
import Timer exposing (Timer, isMore)

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
                , timer = Timer.toggle longBreakTimer
                }
            else
                { model
                | mode = ShortBreak
                , timer = Timer.toggle shortBreakTimer
                }
        ShortBreak ->
            { mode = Work
            , round = model.round + 1
            , timer = Timer.toggle workTimer
            }
        LongBreak ->
            { mode = Work
            , round = 1
            , timer = Timer.toggle workTimer
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
            if isMore model.timer Timer.reset then
                ( { model | timer = Timer.tickDown Timer.second model.timer }, Cmd.none )
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
workTimer : Timer
workTimer = Timer.fromSecond 10         -- dummy value
shortBreakTimer : Timer
shortBreakTimer = Timer.fromSecond 2    -- dummy value
longBreakTimer : Timer
longBreakTimer = Timer.fromSecond 5     -- dummy value

init : () -> ( Model, Cmd Msg )
init _ = 
    ({ mode = Off
     , round = 1
     , timer = workTimer
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