port module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Json.Decode as Json
import Json.Encode
import Model exposing (Action(..), Model, Note, ViewportAction(..), WidthHeight, ZoomAction(..))
import OscilatorType exposing (OscilatorType(..))
import Task exposing (perform)
import View exposing (view)


port noteRelease : Json.Value -> Cmd msg


port notePress : Json.Value -> Cmd msg


port getWaveforms : Json.Value -> Cmd msg


port notePressed : (Json.Encode.Value -> msg) -> Sub msg


port waveforms : (Json.Encode.Value -> msg) -> Sub msg


init : () -> ( Model, Cmd Action )
init () =
    ( { notes = Dict.empty, zoom = 0.25, zoomStart = Nothing, scene = { width = 1, height = 1 }, oscilatorType = Sine }
    , perform (\viewport -> viewport |> ViewportSet |> Viewport) Browser.Dom.getViewport
    )


subscriptions : Model -> Sub Action
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize (\w h -> WidthHeight w h |> ViewportChange |> Viewport)
        , waveforms Waveform
        , notePressed NotePressed
        , Browser.Events.onAnimationFrameDelta AnimationFrame
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


buildReleaseCommand note =
    [ ( "attack", Json.Encode.float note.attack ), ( "node", note.node ) ] |> Json.Encode.object |> noteRelease


decodeNote : Json.Value -> Result Json.Error Note
decodeNote note =
    let
        decoder =
            Json.map6 Note (Json.field "id" Json.int) (Json.field "frequency" Json.float) (Json.field "attack" Json.float) (Json.field "node" Json.value) (Json.succeed []) (Json.succeed Sine)
    in
    note |> Json.decodeValue decoder


type alias WaveformMessage =
    { id : Int, waveform : List Float }


decodeWaveforms forms model =
    let
        decoder =
            Json.map2 WaveformMessage (Json.field "id" Json.int) (Json.field "waveform" (Json.list Json.float))
                |> Json.list

        decodedWaveform =
            Json.decodeValue decoder forms

        updateNote wf note =
            Maybe.map (\n -> { n | waveform = wf }) note

        trim wf =
            wf |> Model.dropToLocalMinimum |> List.take (round model.scene.height)
    in
    case decodedWaveform of
        Ok wfs ->
            { model | notes = List.foldl (\wf notes -> Dict.update wf.id (updateNote (trim wf.waveform)) notes) model.notes wfs }

        Err _ ->
            model


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        ToggleKey i ->
            case Dict.get i model.notes of
                Just note ->
                    ( { model | notes = Dict.remove i model.notes }, buildReleaseCommand note )

                Nothing ->
                    ( model, buildNoteCommand i model.oscilatorType )

        SetOscilatorType oscilatorType ->
            ( { model | oscilatorType = oscilatorType }, Cmd.none )

        Waveform forms ->
            ( decodeWaveforms forms model, Cmd.none )

        NotePressed note ->
            ( case decodeNote note of
                Ok o ->
                    { model | notes = Dict.insert o.id o model.notes }

                Err _ ->
                    model
            , Cmd.none
            )

        Viewport viewPortAction ->
            ( case viewPortAction of
                ViewportSet viewPort ->
                    { model | scene = viewPort.scene }

                ViewportChange viewPort ->
                    { model | scene = { width = toFloat viewPort.width, height = toFloat viewPort.height } }
            , Cmd.none
            )

        Zoom zoom ->
            ( case zoom of
                ZoomStart point ->
                    { model | zoomStart = Just point }

                ZoomChange point ->
                    case model.zoomStart of
                        Nothing ->
                            model

                        Just start ->
                            { model | zoom = (point.x - start.x) / model.scene.width }

                ZoomStop ->
                    { model | zoomStart = Nothing }
            , Cmd.none
            )

        AnimationFrame _ ->
            ( model
            , getWaveforms
                (Json.Encode.list
                    (\note ->
                        Json.Encode.object [ ( "id", Json.Encode.int note.id ), ( "node", note.node ) ]
                    )
                    (Dict.values model.notes)
                )
            )


interval : Int -> Float
interval n =
    2 ^ (toFloat n / 12.0)
