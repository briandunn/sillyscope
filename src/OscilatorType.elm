module OscilatorType exposing (OscilatorType(..), all, fromString, toString)

import Dict exposing (fromList, get)
import Maybe exposing (withDefault)


type OscilatorType
    = Sine
    | Square
    | Sawtooth
    | Triangle


all =
    [ ( "sawtooth", Sawtooth ), ( "sine", Sine ), ( "square", Square ), ( "triangle", Triangle ) ] |> fromList


fromString s =
    all |> get s |> withDefault Sine


toString ot =
    case ot of
        Sawtooth ->
            "sawtooth"

        Sine ->
            "sine"

        Square ->
            "square"

        Triangle ->
            "triangle"
