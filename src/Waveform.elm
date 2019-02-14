module Waveform exposing (autoCorrelate, averageDistance, decodeDataPayload, decodeWaveforms, detectPeaks)

import Array
import Dict exposing (Dict)
import Json.Decode
import Json.Encode
import Model exposing (Analysis, AudioSource, Model, Waveform)


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


updateAnalysis : (Maybe Analysis -> Waveform -> Maybe Analysis) -> Dict Int Waveform -> Dict Int AudioSource -> Dict Int AudioSource
updateAnalysis fn waveforms sources =
    let
        update : Waveform -> AudioSource -> AudioSource
        update waveform audioSource =
            { audioSource | analysis = fn audioSource.analysis waveform }

        fold : ( Int, Waveform ) -> Dict Int AudioSource -> Dict Int AudioSource
        fold ( id, waveform ) s =
            Dict.update id (Maybe.map (update waveform)) s
    in
    waveforms |> Dict.toList |> List.foldr fold sources


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
        correlate shiftedSamples =
            List.foldl (+) 0 (List.map2 (*) samples shiftedSamples)
                :: (case shiftedSamples of
                        [] ->
                            []

                        _ :: rest ->
                            correlate rest
                   )
    in
    correlate samples


detectFrequency : Int -> Waveform -> Float
detectFrequency sampleRate waveform =
    let
        samplesPerRepetition =
            waveform
                |> autoCorrelate
                |> detectPeaks
                |> List.map Tuple.first
                |> averageDistance
    in
    toFloat sampleRate / samplesPerRepetition |> Debug.log "freq"


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

        update analysis waveform =
            let
                wf =
                    trim waveform
            in
            Just { waveform = wf, frequency = detectFrequency model.sampleRate waveform }
    in
    case decodeDataPayload forms of
        Ok wfs ->
            { model
                | audioSources = updateAnalysis update wfs model.audioSources
            }

        Err _ ->
            model
