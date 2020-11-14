module Timer exposing (Timer, going, reset, isLess, tick, second, minute, fromMinute, fromSecond, stop, toggle, continue, restart, map, toString)

type Timer 
    = Going Int     -- Timer is ticking
    | Paused Int    -- Timer is paused


-- Misc
map : (Int -> Int) -> Timer -> Timer
map f timer =
    case timer of
        Going time ->
            Going (f time)
        _ ->
            timer

getTime : Timer -> Int
getTime timer =
    case timer of
        Going time ->
            time
        Paused time -> 
            time

tick : Int -> Timer -> Timer
tick n timer = 
    map (\t -> t + n) timer

-- checks if the first timer is less then the second timer
isLess : Timer -> Timer -> Bool
isLess t1 t2 =
    getTime t1 < getTime t2

going : Timer -> Bool
going timer = 
    case timer of
        Going _ -> 
            True
        _ -> 
            False
    
-- Controls
reset : Timer
reset = Paused 0

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

restart : Timer
restart = 
    Going 0


-- Units
second : Int
second = 1000

minute : Int 
minute = 60000

-- Covert
-- Mostly copied from elm/time
flooredDiv : Int -> Float -> Int
flooredDiv numerator denominator = 
    floor (toFloat numerator / denominator) 

toMinute : Timer -> Int
toMinute timer =
    flooredDiv (getTime timer) (toFloat minute)

toSecond : Timer -> Int
toSecond timer =
    modBy 60 (flooredDiv (getTime timer) 1000)

fromMinute : Int -> Timer
fromMinute n = 
    Paused (n * minute)

fromSecond : Int -> Timer
fromSecond n = 
    Paused (n * second)

toString : Timer -> String
toString timer =
    (String.padLeft 2 '0' <| String.fromInt <| toMinute timer)
    ++ ":" ++
    (String.padLeft 2 '0' <| String.fromInt <| toSecond timer)


