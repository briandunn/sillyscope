port module Worker exposing (main)

import Dict exposing (Dict)
import Json.Decode
import Json.Encode
import Model exposing (Waveform)
import Platform
import Waveform


port data : (Json.Encode.Value -> msg) -> Sub msg


port results : Json.Decode.Value -> Cmd msg


type alias Model =
    {}


type Action
    = Data Json.Encode.Value


encodeFrequencies : Dict Int Float -> Json.Encode.Value
encodeFrequencies waveforms =
    waveforms
        |> Dict.toList
        |> Json.Encode.list (\( k, v ) -> Json.Encode.object [ ( "id", Json.Encode.int k ), ( "data", Json.Encode.float v ) ])


detectFrequencies : Dict Int Waveform -> Dict Int Float
detectFrequencies dict =
    Dict.map (\_ waveform -> Waveform.samplesPerRepetition waveform) dict


init : () -> ( Model, Cmd a )
init _ =
    ( Model, Cmd.none )


update message _ =
    case message of
        Data payload ->
            ( Model, payload |> Waveform.decodeDataPayload |> Result.withDefault Dict.empty |> detectFrequencies |> encodeFrequencies |> results )


subscriptions _ =
    data Data


main =
    Platform.worker { init = init, update = update, subscriptions = subscriptions }
