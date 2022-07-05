module MainTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (getLetterClass)
import Test exposing (..)


suite : Test
suite =
    let
        word =
            "random"
    in
    describe "Wordle module"
        [ describe "getLetterClass"
            [ test "Get correct class on right position" <|
                \_ ->
                    Expect.equal "success" (getLetterClass 0 'r' word)
            , test "Get incorrect-position class on exist but wrong position" <|
                \_ ->
                    Expect.equal "incorrect-position" (getLetterClass 4 'r' word)
            , test "Get wrong when position is not correct" <|
                \_ ->
                    Expect.equal "wrong" (getLetterClass 4 'e' word)
            ]
        ]
