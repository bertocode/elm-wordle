module MainTest exposing (..)

import Array
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (KeyboardKeyState(..), LetterInWord(..), getKeyBoardColor, getLetterState)
import Test exposing (..)


suite : Test
suite =
    let
        word =
            "random"

        wordsProven =
            Array.fromList [ "renmod", "", "", "", "", "" ]

        indexOfWords =
            1
    in
    describe "Wordle module"
        [ describe "getLetterState"
            [ test "Get correct on right position" <|
                \_ ->
                    Expect.equal Correct (getLetterState 0 'r' word)
            , test "Get incorrect-position  on exist but wrong position" <|
                \_ ->
                    Expect.equal BadOrder (getLetterState 4 'r' word)
            , test "Get wrong when the letter is not in the word" <|
                \_ ->
                    Expect.equal Incorrect (getLetterState 4 'e' word)
            ]
        , describe "getKeyBoardColor"
            [ test "The key is written in the right order previously" <|
                \_ ->
                    Expect.equal InOrder (getKeyBoardColor 'r' indexOfWords wordsProven word)
            , test "The key is written but never in order previously" <|
                \_ ->
                    Expect.equal NotInOrder (getKeyBoardColor 'm' indexOfWords wordsProven word)
            , test "The key has proven and it is not in the word" <|
                \_ ->
                    Expect.equal NotInWord (getKeyBoardColor 'e' indexOfWords wordsProven word)
            , test "The key has not been proven yet" <|
                \_ ->
                    Expect.equal NotUsed (getKeyBoardColor 'q' indexOfWords wordsProven word)
            ]
        ]
