module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown)
import Char exposing (isAlpha)
import Html exposing (Html, div, h1, header, main_, node, section, text)
import Html.Attributes exposing (class, href, rel)
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


getLetterClass : Int -> Char -> String -> String
getLetterClass currentPosition char wordToGuess =
    let
        charToString =
            String.fromList [ char ]

        indexes =
            String.indexes charToString wordToGuess
    in
    if List.member currentPosition indexes then
        "success"

    else if String.contains charToString wordToGuess then
        "incorrect-position"

    else
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
                                        div [ class ("letter " ++ getLetterClass letterIndex letter model.wordToGuess) ]
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
            ]
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
