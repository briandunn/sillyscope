port module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Json.Decode
import Json.Encode
import Model exposing (Action(..), AudioSource, Model, ViewportAction(..), Waveform, WidthHeight, ZoomAction(..), init, micId)
import Ports exposing (decodeAudioSource, encodeGetAnalysisCommand, encodeNoteCommand, encodeReleaseCommand)
import Task exposing (attempt)
import View exposing (view)
import Waveform exposing (decodeFfts, decodeWaveforms)


port releaseAudioSource : Json.Decode.Value -> Cmd msg


port notePress : Json.Decode.Value -> Cmd msg


port getWaveforms : Json.Decode.Value -> Cmd msg


port getFfts : Json.Decode.Value -> Cmd msg


port activateMic : Json.Decode.Value -> Cmd msg


port addAudioSource : (Json.Encode.Value -> msg) -> Sub msg


port waveforms : (Json.Encode.Value -> msg) -> Sub msg


port ffts : (Json.Encode.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Action
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize (\w h -> WidthHeight w h |> ViewportChange |> Viewport)
        , waveforms UpdateWaveform
        , addAudioSource AddAudioSource
        , ffts UpdateFfts
        ]


noteIdToFreq n =
    (n + 3) |> interval |> (*) 220


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias FreqMessage =
    { id : Int, freq : Int }


buildGetWaveFormsCommand { audioSources } =
    let
        payload =
            encodeGetAnalysisCommand audioSources
    in
    [ getWaveforms, getFfts ]
        |> List.map (\fn -> fn payload)
        |> Cmd.batch


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        ToggleKey i ->
            toggleAudioSource i model |> Maybe.withDefault ( model, encodeNoteCommand i (noteIdToFreq i) model.oscilatorType |> notePress )

        SetOscilatorType oscilatorType ->
            ( { model | oscilatorType = oscilatorType }, Cmd.none )

        UpdateWaveform forms ->
            ( decodeWaveforms forms model
            , if Dict.isEmpty model.audioSources then
                Cmd.none

              else
                model.audioSources |> encodeGetAnalysisCommand |> getWaveforms
            )

        UpdateFfts fft ->
            ( decodeFfts fft model
            , if Dict.member micId model.audioSources then
                model.audioSources |> encodeGetAnalysisCommand |> getFfts

              else
                Cmd.none
            )

        AddAudioSource note ->
            case decodeAudioSource note of
                Ok o ->
                    let
                        m =
                            { model | audioSources = Dict.insert o.id o model.audioSources }
                    in
                    ( m, buildGetWaveFormsCommand m )

                Err _ ->
                    ( model, Cmd.none )

        Viewport viewPortAction ->
            let
                getWrapper =
                    "scope-wrapper" |> Browser.Dom.getElement |> attempt (\result -> Viewport (WrapperElement result))
            in
            case viewPortAction of
                WrapperElement result ->
                    ( { model | wrapperElement = Result.toMaybe result }, Cmd.none )

                ViewportSet viewPort ->
                    ( model, getWrapper )

                ViewportChange viewPort ->
                    ( model, getWrapper )

        Zoom zoom ->
            ( case zoom of
                ZoomStart point ->
                    { model | zoomStart = Just point }

                ZoomChange point ->
                    case model.zoomStart of
                        Nothing ->
                            model

                        Just start ->
                            case model.wrapperElement of
                                Nothing ->
                                    model

                                Just element ->
                                    { model | zoom = (point.x - start.x) / element.element.width }

                ZoomStop ->
                    { model | zoomStart = Nothing }
            , Cmd.none
            )

        ToggleMic ->
            toggleAudioSource micId model
                |> Maybe.withDefault ( model, [ ( "id", Json.Encode.int micId ) ] |> Json.Encode.object |> activateMic )


toggleAudioSource : Int -> Model -> Maybe ( Model, Cmd a )
toggleAudioSource id model =
    model.audioSources
        |> Dict.get id
        |> Maybe.map
            (\audioSource ->
                ( { model
                    | audioSources = Dict.remove id model.audioSources
                  }
                , encodeReleaseCommand audioSource |> releaseAudioSource
                )
            )


interval : Int -> Float
interval n =
    2 ^ (toFloat n / 12.0)
