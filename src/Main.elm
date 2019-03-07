port module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Json.Decode
import Json.Encode
import Model exposing (Action(..), AudioSource, Model, ViewportAction(..), Waveform, WidthHeight, ZoomAction(..), init, micId, noteIdToFreq)
import Ports exposing (decodeAudioSource, encodeGetAnalysisCommand, encodeNoteCommand, encodeReleaseCommand)
import Task exposing (attempt)
import View exposing (view)
import Waveform exposing (decodeWaveforms)


port activateMic : Json.Decode.Value -> Cmd msg


port addAudioSource : (Json.Encode.Value -> msg) -> Sub msg


port calculateFrequencies : Json.Decode.Value -> Cmd msg


port getWaveforms : Json.Decode.Value -> Cmd msg


port notePress : Json.Decode.Value -> Cmd msg


port frequencies : (Json.Encode.Value -> msg) -> Sub msg


port releaseAudioSource : Json.Decode.Value -> Cmd msg


port waveforms : (Json.Encode.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Action
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize (\w h -> WidthHeight w h |> ViewportChange |> Viewport)
        , waveforms UpdateWaveform
        , addAudioSource AddAudioSource
        , frequencies UpdateFrequency
        ]


main =
    Browser.element
        { init = Json.Decode.decodeValue (Json.Decode.field "sampleRate" Json.Decode.int) >> Result.withDefault 0 >> init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


encodeCalculateFrequenciesCommand id waveform =
    Json.Encode.list
        (\( i, w ) ->
            Json.Encode.object
                [ ( "id", Json.Encode.int i )
                , ( "data", Json.Encode.list Json.Encode.float w )
                ]
        )
        [ ( id, waveform ) ]


type alias Frequencies =
    { id : Int, data : Float }


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
                Cmd.batch
                    ((model.audioSources |> encodeGetAnalysisCommand |> getWaveforms)
                        :: (model.audioSources
                                |> Dict.get micId
                                |> Maybe.map
                                    (.waveform
                                        >> Maybe.map
                                            (encodeCalculateFrequenciesCommand micId
                                                >> calculateFrequencies
                                                >> List.singleton
                                            )
                                        >> Maybe.withDefault []
                                    )
                                |> Maybe.withDefault []
                           )
                    )
            )

        UpdateFrequency payload ->
            let
                decodedFrequencies =
                    payload
                        |> Json.Decode.decodeValue
                            (Json.Decode.list
                                (Json.Decode.map2
                                    Frequencies
                                    (Json.Decode.field "id" Json.Decode.int)
                                    (Json.Decode.field "data" Json.Decode.float)
                                )
                            )
            in
            ( model, Cmd.none )

        AddAudioSource note ->
            case decodeAudioSource note of
                Ok o ->
                    let
                        m =
                            { model | audioSources = Dict.insert o.id o model.audioSources }
                    in
                    ( m, m.audioSources |> encodeGetAnalysisCommand |> getWaveforms )

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
