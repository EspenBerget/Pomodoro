module TimerTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Test exposing (..)

import Timer exposing (..)


suite : Test
suite =
    describe "The Timer module"
        [ describe "toMinute and fromMinute"
            [ fuzz (intRange 0 59) "composing toMinute and fromMinute returns the same value" <|
                \n -> Expect.equal n (toMinute <| fromMinute n)
            , fuzz int "result always smaller than 60" <|
                \n -> Expect.lessThan 60 (toMinute <| fromMinute n)
            , fuzz int "result never negative" <|
                \n -> Expect.atLeast 0 (toMinute <| fromMinute n)
            ]
        , describe "toSecond and fromSecond"
            [ fuzz (intRange 0 59) "composing toSecond and fromSecond returns the same value" <|
                \n -> Expect.equal n (toSecond <| fromSecond n)
            , fuzz int "result always smaller than 60" <|
                \n -> Expect.lessThan 60 (toSecond <| fromSecond n)
            , fuzz int "result never negative" <|
                \n -> Expect.atLeast 0 (toSecond <| fromSecond n)
            ]
        , describe "toString"
            [ test "should display correct value" <|
                \n -> Expect.equal "23:56" (toString <| add (fromMinute 23) (fromSecond 56))
            , test "should display valid time value even if input is invalid" <|
                \n -> Expect.equal "05:00" (toString <| fromMinute 125)
            ]
        ]
