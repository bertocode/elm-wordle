module Test.Generated.Main exposing (main)

import MainTest

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    Test.Runner.Node.run
        { runs = 100
        , report = ConsoleReport UseColor
        , seed = 28375823973126
        , processes = 12
        , globs =
            []
        , paths =
            [ "/home/berto/Repos/elm-wordle/tests/MainTest.elm"
            ]
        }
        [ ( "MainTest"
          , [ Test.Runner.Node.check MainTest.suite
            ]
          )
        ]