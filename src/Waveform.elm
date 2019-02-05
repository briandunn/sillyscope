module Waveform exposing (decodeFfts, decodeWaveforms, dropToLocalMinimum)

import Dict exposing (Dict)
import Json.Decode
import Json.Encode
import Model exposing (AudioSource, Waveform)



-- import Model exposing (Model)


type alias WaveformMessage =
    { id : Int, data : Waveform }


dropWhileFirstTwo test list =
    case list of
        first :: second :: tail ->
            if test first second then
                dropWhileFirstTwo test (second :: tail)

            else
                list

        first :: tail ->
            list

        [] ->
            []


dropToLocalMinimum : Waveform -> Waveform
dropToLocalMinimum values =
    values
        |> dropWhileFirstTwo (\a b -> (a > 0 && b > 0) || (a <= 0 && b <= 0))
        |> List.drop 1
        |> dropWhileFirstTwo (\a b -> a > 0 && b > 0)


decodeDataPayload : Json.Decode.Value -> Result Json.Decode.Error (Dict Int Waveform)
decodeDataPayload payload =
    let
        decoder =
            Json.Decode.map2
                WaveformMessage
                (Json.Decode.field "id" Json.Decode.int)
                (Json.Decode.field "data" (Json.Decode.list Json.Decode.float))
                |> Json.Decode.list

        fold messages =
            messages |> List.map (\{ id, data } -> ( id, data )) |> Dict.fromList
    in
    Json.Decode.decodeValue decoder payload |> Result.map fold


mapDict : (a -> b) -> Dict comparable a -> Dict comparable b
mapDict fn dict =
    Dict.foldl (\k v b -> Dict.insert k (fn v) b) Dict.empty dict



-- decodeWaveforms : Json.Decode.Value -> Model -> Model


decodeWaveforms forms model =
    let
        frameCount =
            case model.wrapperElement of
                Nothing ->
                    0

                Just element ->
                    round (element.element.width * model.zoom)

        trim : Waveform -> Waveform
        trim =
            dropToLocalMinimum >> List.take frameCount

        update : Waveform -> AudioSource -> AudioSource
        update waveform audioSource =
            { audioSource | analysis = Just { waveform = trim waveform } }

        fold : ( Int, Waveform ) -> Dict Int AudioSource -> Dict Int AudioSource
        fold ( id, waveform ) sources =
            Dict.update id (Maybe.map (update waveform)) sources

        addWaveforms : Dict Int AudioSource -> Dict Int Waveform -> Dict Int AudioSource
        addWaveforms sources waveforms =
            waveforms |> Dict.toList |> List.foldr fold sources
    in
    case decodeDataPayload forms of
        Ok wfs ->
            { model | audioSources = addWaveforms model.audioSources wfs }

        Err _ ->
            model



-- decodeFfts : Json.Decode.Value -> Model -> Model


dominant count =
    \values ->
        let
            valueCount =
                values |> List.length |> toFloat
        in
        values
            |> List.indexedMap Tuple.pair
            |> List.sortBy Tuple.second
            |> List.take count
            |> List.map (\( i, v ) -> toFloat i / valueCount)


decodeFfts ffts model =
    case decodeDataPayload ffts of
        Ok f ->
            -- { model | ffts = mapDict (dominant 12) f }
            model

        Err _ ->
            model
