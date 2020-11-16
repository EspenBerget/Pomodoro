module Timer exposing ( Timer
                      , isMore
                      , getTime
                      , add
                      , toSecond
                      , toMinute
                      , going
                      , reset
                      , isLess
                      , tickUp
                      , tickDown
                      , second
                      , minute
                      , fromMinute
                      , fromSecond
                      , stop
                      , toggle
                      , continue
                      , restart
                      , map
                      , toString
                      )

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

tickUp : Int -> Timer -> Timer
tickUp n timer = 
    map (\t -> t + n) timer

tickDown : Int -> Timer -> Timer
tickDown n timer = 
    map (\t -> t - n) timer

isLess : Timer -> Timer -> Bool
isLess t1 t2 =
    getTime t1 < getTime t2

isMore : Timer -> Timer -> Bool
isMore t1 t2 =
    getTime t1 > getTime t2

going : Timer -> Bool
going timer = 
    case timer of
        Going _ -> 
            True
        _ -> 
            False

add : Timer -> Timer -> Timer
add t1 t2 = 
    case t1 of
        Going t -> 
            Going (t + getTime t2)
        Paused t ->
            Paused (t + getTime t2)
    
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
-- inspired by elm/time
flooredDiv : Int -> Float -> Int
flooredDiv numerator denominator = 
    floor (toFloat numerator / denominator) 

toMinute : Timer -> Int
toMinute timer =
    modBy 60 (flooredDiv (getTime timer) (toFloat minute))

toSecond : Timer -> Int
toSecond timer =
    modBy 60 (flooredDiv (getTime timer) (toFloat second))

fromMinute : Int -> Timer
fromMinute n = 
    Paused (abs n * minute)

fromSecond : Int -> Timer
fromSecond n = 
    Paused (abs n * second)

toString : Timer -> String
toString timer =
    (String.padLeft 2 '0' <| String.fromInt <| toMinute timer)
    ++ ":" ++
    (String.padLeft 2 '0' <| String.fromInt <| toSecond timer)


