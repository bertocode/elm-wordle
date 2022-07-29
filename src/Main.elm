module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onClick, onKeyDown)
import Browser.Navigation exposing (Key)
import Char exposing (isAlpha)
import Html exposing (Html, a, div, h1, header, main_, node, section, text)
import Html.Attributes exposing (class, href, rel)
import Html.Events
import Json.Decode as D
import Set exposing (Set)



---- MODEL ----


type alias Model =
    { wordToGuess : String
    , usedKeys : Set Char
    , currentTypedWord : Int
    , wordsUsed : Array String
    , wordGuessed : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { wordToGuess = "random"
      , usedKeys = Set.empty
      , currentTypedWord = 0
      , wordsUsed = Array.repeat 6 ""
      , wordGuessed = False
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Typed Char
    | PressRemove
    | TryWord
    | NoOp


type KeyboardKeyState
    = InOrder
    | NotInOrder
    | NotInWord
    | NotUsed


type LetterInWord
    = Correct
    | BadOrder
    | Incorrect


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Typed keycode ->
            let
                typedChar =
                    keycode

                lastWordUsed =
                    model.wordsUsed
                        |> Array.get model.currentTypedWord
                        |> Maybe.withDefault ""

                newWord =
                    if String.length lastWordUsed < String.length model.wordToGuess then
                        String.append lastWordUsed (String.fromList [ typedChar ])
                            |> String.toLower

                    else
                        lastWordUsed

                newWords =
                    model.wordsUsed
                        |> Array.set model.currentTypedWord newWord
            in
            if isAlpha typedChar then
                ( { model | wordsUsed = newWords }, Cmd.none )

            else
                ( model, Cmd.none )

        PressRemove ->
            let
                newWord =
                    model.wordsUsed
                        |> Array.get model.currentTypedWord
                        |> Maybe.withDefault ""
                        |> String.dropRight 1

                newWords =
                    model.wordsUsed
                        |> Array.set model.currentTypedWord newWord
            in
            ( { model | wordsUsed = newWords }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        TryWord ->
            let
                wordToCheck =
                    Array.get model.currentTypedWord model.wordsUsed
                        |> Maybe.withDefault ""

                isWordReady =
                    wordToCheck
                        |> String.length
                        |> (==) (String.length model.wordToGuess)
            in
            if isWordReady then
                if model.wordToGuess == wordToCheck then
                    ( { model | currentTypedWord = model.currentTypedWord + 1, wordGuessed = True }, Cmd.none )

                else
                    ( { model | currentTypedWord = model.currentTypedWord + 1 }, Cmd.none )

            else
                ( model, Cmd.none )



---- VIEW ----


getLetterState : Int -> Char -> String -> LetterInWord
getLetterState currentPosition char wordToGuess =
    let
        charToString =
            String.fromList [ char ]

        indexes =
            String.indexes charToString wordToGuess
    in
    if List.member currentPosition indexes then
        Correct

    else if String.contains charToString wordToGuess then
        BadOrder

    else
        Incorrect


letterInWordToClass : LetterInWord -> String
letterInWordToClass letterInWord =
    case letterInWord of
        Correct ->
            "success"

        BadOrder ->
            "incorrect-position"

        Incorrect ->
            "wrong"


view : Model -> Html Msg
view model =
    let
        guessedWordLength =
            String.length model.wordToGuess
    in
    div []
        [ node "link" [ rel "stylesheet", href "/styles.css" ] []
        , header []
            [ h1 [] [ text "Play wordle" ]
            ]
        , main_ []
            [ section [ class "grid" ] <|
                (Array.indexedMap
                    (\index word ->
                        div [ class "word" ] <|
                            if index < model.currentTypedWord then
                                List.indexedMap
                                    (\letterIndex letter ->
                                        div [ class ("letter " ++ (getLetterState letterIndex letter model.wordToGuess |> letterInWordToClass)) ]
                                            [ text (String.fromList [ letter ])
                                            ]
                                    )
                                    (word
                                        |> String.toList
                                    )

                            else
                                let
                                    filledWord =
                                        String.padRight guessedWordLength ' ' word
                                            |> String.toList
                                in
                                List.map
                                    (\letter ->
                                        div [ class "letter" ]
                                            [ text (String.fromList [ letter ])
                                            ]
                                    )
                                    filledWord
                    )
                    model.wordsUsed
                    |> Array.toList
                )
            , section [ class "keyboard" ] <|
                List.map
                    (\keyboardLine ->
                        div [ class "keyboard-line" ] <|
                            List.map
                                (\letter ->
                                    a
                                        [ class <| "letter " ++ (getKeyBoardColor letter model.currentTypedWord model.wordsUsed model.wordToGuess |> keyboardKeyStateToClassColor)
                                        , href "#"
                                        , Html.Events.onClick <| Typed letter
                                        ]
                                        [ text (String.fromList [ letter ]) ]
                                )
                                (keyboardLine |> String.toList)
                    )
                    keyboardLetters
            ]
        ]


keyboardKeyStateToClassColor : KeyboardKeyState -> String
keyboardKeyStateToClassColor keyState =
    case keyState of
        NotUsed ->
            ""

        NotInOrder ->
            "incorrect-position"

        NotInWord ->
            "not-in-word"

        InOrder ->
            "success"


getKeyBoardColor : Char -> Int -> Array String -> String -> KeyboardKeyState
getKeyBoardColor currentKey currentIndex arrayOfWords wordToGuess =
    let
        charAsString =
            String.fromList [ currentKey ]

        availableWords =
            Array.slice 0 currentIndex arrayOfWords

        keyExistInCurrent =
            Array.foldl
                (\word acc ->
                    String.contains charAsString word || acc
                )
                False
                availableWords

        keyInWord =
            Array.foldl
                (\word acc ->
                    String.contains charAsString wordToGuess || acc
                )
                False
                availableWords

        -- salm|o|n | carl|o|s
        keyInOrder =
            Array.foldl
                (\word acc ->
                    List.append (String.indexes charAsString word) acc
                )
                []
                availableWords
                |> Set.fromList
                |> Set.foldl (\el acc -> List.member el (String.indexes charAsString wordToGuess) || acc) False
    in
    case ( keyExistInCurrent, keyInWord, keyInOrder ) of
        ( True, True, True ) ->
            InOrder

        ( True, True, False ) ->
            NotInOrder

        ( False, _, _ ) ->
            NotUsed

        ( True, False, _ ) ->
            NotInWord


keyboardLetters : List String
keyboardLetters =
    [ "qwertyuiop"
    , "asdfghjkl"
    , "zxcvbnm"
    ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown <|
            (D.field "key" D.string
                |> D.andThen
                    (\pKey ->
                        if model.wordGuessed then
                            D.succeed NoOp

                        else
                            case String.uncons pKey of
                                Just ( char, "" ) ->
                                    D.succeed <| Typed char

                                _ ->
                                    case pKey of
                                        "Backspace" ->
                                            D.succeed PressRemove

                                        "Enter" ->
                                            D.succeed TryWord

                                        _ ->
                                            D.succeed NoOp
                    )
            )
        ]
