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


encodeWaveforms : Result Json.Decode.Error (Dict Int Waveform) -> Json.Encode.Value
encodeWaveforms waveforms =
    waveforms
        |> Result.withDefault Dict.empty
        |> Dict.toList
        |> Json.Encode.list (\( k, v ) -> Json.Encode.object [ ( "id", Json.Encode.int k ), ( "data", Json.Encode.list Json.Encode.float v ) ])


init : () -> ( Model, Cmd a )
init _ =
    ( Model, Cmd.none )


update message _ =
    case message of
        Data payload ->
            ( Model, payload |> Waveform.decodeDataPayload |> encodeWaveforms |> results )


subscriptions _ =
    data Data


main =
    Platform.worker { init = init, update = update, subscriptions = subscriptions }
