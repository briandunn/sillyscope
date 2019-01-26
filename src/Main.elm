port module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Json.Decode as Json
import Json.Encode
import Model exposing (Action(..), AudioSource, Model, ViewportAction(..), Waveform, WidthHeight, ZoomAction(..), micId)
import OscilatorType exposing (OscilatorType(..))
import Task exposing (attempt, perform)
import View exposing (view)


port releaseAudioSource : Json.Value -> Cmd msg


port notePress : Json.Value -> Cmd msg


port getWaveforms : Json.Value -> Cmd msg


port getFfts : Json.Value -> Cmd msg


port activateMic : Json.Value -> Cmd msg


port addAudioSource : (Json.Encode.Value -> msg) -> Sub msg


port addDominantFreq : (Json.Encode.Value -> msg) -> Sub msg


port waveforms : (Json.Encode.Value -> msg) -> Sub msg


port ffts : (Json.Encode.Value -> msg) -> Sub msg


init : () -> ( Model, Cmd Action )
init () =
    ( { waveforms = Dict.empty, audioSources = Dict.empty, zoom = 1, zoomStart = Nothing, wrapperElement = Nothing, oscilatorType = Sine }
    , perform (\viewport -> viewport |> ViewportSet |> Viewport) Browser.Dom.getViewport
    )


subscriptions : Model -> Sub Action
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize (\w h -> WidthHeight w h |> ViewportChange |> Viewport)
        , waveforms UpdateWaveform
        , addAudioSource AddAudioSource
        , ffts UpdateFfts
        , addDominantFreq AddDominantFreq
        ]


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


buildNoteCommand n ot =
    let
        -- middle c
        freq =
            (n + 3) |> interval |> (*) 220

        oscilatorType =
            OscilatorType.toString ot

        json =
            Json.Encode.object
                [ ( "id", Json.Encode.int n )
                , ( "frequency", Json.Encode.float freq )
                , ( "attack", Json.Encode.float 0.1 )
                , ( "type", Json.Encode.string oscilatorType )
                ]
    in
    json |> notePress


buildReleaseCommand { node } =
    [ ( "release", Json.Encode.float 0.5 ), ( "node", node ) ] |> Json.Encode.object |> releaseAudioSource


decodeNote : Json.Value -> Result Json.Error AudioSource
decodeNote note =
    let
        decoder =
            Json.map2 AudioSource (Json.field "id" Json.int) (Json.field "node" Json.value)
    in
    note |> Json.decodeValue decoder


type alias WaveformMessage =
    { id : Int, data : Waveform }


decodeDataPayload : Json.Value -> Result Json.Error (Dict Int Waveform)
decodeDataPayload payload =
    let
        decoder =
            Json.map2 WaveformMessage (Json.field "id" Json.int) (Json.field "data" (Json.list Json.float))
                |> Json.list

        fold messages =
            messages |> List.map (\{ id, data } -> ( id, data )) |> Dict.fromList
    in
    Json.decodeValue decoder payload |> Result.map fold


mapDict : (a -> b) -> Dict comparable a -> Dict comparable b
mapDict fn dict =
    Dict.foldl (\k v b -> Dict.insert k (fn v) b) Dict.empty dict


decodeWaveforms forms model =
    let
        frameCount =
            case model.wrapperElement of
                Nothing ->
                    0

                Just element ->
                    round (element.element.width * model.zoom)

        trim : Waveform -> Waveform
        trim wf =
            wf |> Model.dropToLocalMinimum |> List.take frameCount
    in
    case decodeDataPayload forms of
        Ok wfs ->
            { model | waveforms = mapDict trim wfs }

        Err _ ->
            model


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        ToggleKey i ->
            toggleAudioSource i model |> Maybe.withDefault ( model, buildNoteCommand i model.oscilatorType )

        SetOscilatorType oscilatorType ->
            ( { model | oscilatorType = oscilatorType }, Cmd.none )

        UpdateWaveform forms ->
            ( decodeWaveforms forms model, buildGetWaveformsCommand model.audioSources )

        UpdateFfts fft ->
            case decodeDataPayload fft of
                Err _ ->
                    ( model, Cmd.none )

                Ok data ->
                    --   ( { model | ffts = Dict. id data model.ffts }, getFfts )
                    ( model, Json.Encode.array [] |> getFfts )

        AddAudioSource note ->
            case decodeNote note of
                Ok o ->
                    let
                        audioSources =
                            Dict.insert o.id o model.audioSources
                    in
                    ( { model | audioSources = audioSources }, buildGetWaveformsCommand audioSources )

                Err _ ->
                    ( model, Cmd.none )

        AddDominantFreq freq ->
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
                , buildReleaseCommand audioSource
                )
            )


buildGetWaveformsCommand notes =
    case Dict.values notes of
        [] ->
            Cmd.none

        values ->
            values
                |> Json.Encode.list
                    (\note ->
                        Json.Encode.object [ ( "id", Json.Encode.int note.id ), ( "node", note.node ) ]
                    )
                |> getWaveforms


interval : Int -> Float
interval n =
    2 ^ (toFloat n / 12.0)
