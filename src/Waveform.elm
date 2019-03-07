module Waveform exposing (autoCorrelate, averageDistance, decodeDataPayload, decodeWaveforms, detectFrequency, detectPeaks, samplesPerRepetition)

import Array
import Dict exposing (Dict)
import Json.Decode
import Json.Encode
import Model exposing (AudioSource, Model, Waveform)


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


updateAudioSources : (AudioSource -> b -> AudioSource) -> Dict Int b -> Dict Int AudioSource -> Dict Int AudioSource
updateAudioSources fn data sources =
    let
        skip _ _ r =
            r

        both id s d r =
            Dict.insert id (fn d s) r
    in
    Dict.merge skip both skip data sources sources


detectPeaks samples =
    let
        detectPeaks_ i ({ peaks, skip, threshold } as memo) samples_ =
            let
                detect first second =
                    if (second - first) >= 0 then
                        -- positive slope or flat
                        { memo | skip = False }

                    else if skip || first < threshold then
                        memo

                    else
                        { memo | peaks = peaks ++ [ ( i, first ) ], skip = True, threshold = 0.5 * first }
            in
            case samples_ of
                first :: second :: rest ->
                    detectPeaks_ (i + 1)
                        (detect first second)
                        (second :: rest)

                _ ->
                    peaks
    in
    detectPeaks_ 0 { peaks = [], skip = False, threshold = 0 } samples


normalize signal =
    let
        fold sample m =
            if abs sample > m then
                abs sample

            else
                m

        max =
            List.foldl fold 0 signal

        normalizeSample sample =
            sample / max
    in
    List.map normalizeSample signal


averageDistance : List Int -> Float
averageDistance peakIndexes =
    let
        spacing list =
            case list of
                first :: second :: rest ->
                    second - first :: spacing (second :: rest)

                _ ->
                    []

        distances =
            spacing peakIndexes
    in
    (distances |> List.sum |> toFloat) / (distances |> List.length |> toFloat)


autoCorrelate : Waveform -> Waveform
autoCorrelate samples =
    let
        correlate acc shiftedSamples =
            case shiftedSamples of
                [] ->
                    acc |> List.reverse

                _ :: rest ->
                    correlate
                        (List.foldl (+) 0 (List.map2 (*) samples shiftedSamples)
                            :: acc
                        )
                        rest
    in
    correlate [] samples


samplesPerRepetition : Waveform -> Float
samplesPerRepetition =
    autoCorrelate >> detectPeaks >> List.map Tuple.first >> averageDistance


detectFrequency : Int -> Waveform -> Float
detectFrequency sampleRate waveform =
    toFloat sampleRate / samplesPerRepetition waveform


decodeWaveforms : Json.Decode.Value -> Model -> Model
decodeWaveforms forms model =
    let
        frameCount : Int
        frameCount =
            case model.wrapperElement of
                Nothing ->
                    0

                Just element ->
                    round (element.element.width * model.zoom)

        trim : Waveform -> Waveform
        trim =
            dropToLocalMinimum >> List.take frameCount

        update source waveform =
            let
                wf =
                    trim waveform
            in
            { source | waveform = Just wf }
    in
    case decodeDataPayload forms of
        Ok wfs ->
            { model
                | audioSources = updateAudioSources update wfs model.audioSources
            }

        Err _ ->
            model
