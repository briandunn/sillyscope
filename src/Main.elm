port module Main exposing (Action(..), Model, PathCommand(..), colors, commandToString, interval, main, noteWave, pathDefinition, update, view)

import Array exposing (Array)
import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Html exposing (div, input)
import Html.Attributes exposing (style, type_)
import Html.Events exposing (on, onClick)
import Json.Decode as Json
import Json.Encode
import Set exposing (Set)
import String
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, fill, height, stroke, strokeOpacity, strokeWidth, viewBox, width)
import Task


type alias Note =
    { id : Int, frequency : Float, attack : Float, node : Json.Value, waveform : Array Float }


type alias Model =
    { notes : Dict Int Note
    , zoom : Float
    , zoomStart : Maybe Point
    , scene : { width : Float, height : Float }
    }


type alias Point =
    { x : Float, y : Float }


type alias WidthHeight =
    { width : Int, height : Int }


type ViewportAction
    = ViewportChange WidthHeight
    | ViewportSet Browser.Dom.Viewport


type ZoomAction
    = ZoomStop
    | ZoomStart Point
    | ZoomChange Point


type Action
    = ToggleKey Int
    | Zoom ZoomAction
    | Viewport ViewportAction
    | Waveform Json.Encode.Value
    | NotePressed Json.Encode.Value


port noteRelease : Json.Value -> Cmd msg


port notePress : Json.Value -> Cmd msg


port notePressed : (Json.Encode.Value -> msg) -> Sub msg


port waveform : (Json.Encode.Value -> msg) -> Sub msg


init : () -> ( Model, Cmd Action )
init () =
    ( { notes = Dict.empty, zoom = 0.25, zoomStart = Nothing, scene = { width = 1, height = 1 } }
    , Task.perform (\viewport -> viewport |> ViewportSet |> Viewport) Browser.Dom.getViewport
    )


subscriptions : Model -> Sub Action
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize (\w h -> WidthHeight w h |> ViewportChange |> Viewport)
        , waveform Waveform
        , notePressed NotePressed
        ]


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


buildNoteCommand n =
    let
        -- middle c
        freq =
            (n + 3) |> interval |> (*) 220

        json =
            Json.Encode.object
                [ ( "id", Json.Encode.int n )
                , ( "frequency", Json.Encode.float freq )
                , ( "attack", Json.Encode.float 0.1 )
                , ( "type", Json.Encode.string "sine" )
                ]
    in
    json |> notePress


buildReleaseCommand note =
    [ ( "attack", Json.Encode.float note.attack ), ( "node", note.node ) ] |> Json.Encode.object |> noteRelease


decodeNote : Json.Value -> Result Json.Error Note
decodeNote note =
    let
        decoder =
            Json.map5 Note (Json.field "id" Json.int) (Json.field "frequency" Json.float) (Json.field "attack" Json.float) (Json.field "node" Json.value) (Json.succeed Array.empty)
    in
    note |> Json.decodeValue decoder


type alias WaveformMessage =
    { id : Int, waveform : Array Float }


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        ToggleKey i ->
            case Dict.get i model.notes of
                Just note ->
                    ( { model | notes = Dict.remove i model.notes }, buildReleaseCommand note )

                Nothing ->
                    ( model, buildNoteCommand i )

        Waveform form ->
            ( let
                decoder =
                    Json.map2 WaveformMessage (Json.field "id" Json.int) (Json.field "waveform" (Json.array Json.float))

                decodedWaveform =
                    Json.decodeValue decoder form

                updateNote : Array Float -> Maybe Note -> Maybe Note
                updateNote wf note =
                    Maybe.map (\n -> { n | waveform = wf }) note
              in
              case decodedWaveform of
                Ok wf ->
                    { model | notes = Dict.update wf.id (updateNote wf.waveform) model.notes }

                Err _ ->
                    model
            , Cmd.none
            )

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


type PathCommand
    = M Point
    | L Point


commandToString : PathCommand -> String
commandToString command =
    let
        toString c p =
            c ++ String.join "," (List.map String.fromFloat [ p.x, p.y ])
    in
    case command of
        M p ->
            toString "M" p

        L p ->
            toString "L" p


pathDefinition : List PathCommand -> String
pathDefinition commands =
    commands |> List.map commandToString |> String.join " "


interval : Int -> Float
interval n =
    2 ^ (toFloat n / 12.0)


colors =
    [ "#fcbeed", "#fa9fea", "#f580f0", "#dd63ee", "#ac4be5", "#6937d8", "#2737c8", "#1b79b4", "#129b7c", "#0b7e18", "#375e07", "#3d3404", "#fcbeed" ]


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


dropToLocalMinimum : List Float -> List Float
dropToLocalMinimum values =
    values
        |> dropWhileFirstTwo (>)
        |> List.drop 1
        |> dropWhileFirstTwo (<)
        |> List.drop 1


noteWave : String -> Float -> Float -> Note -> Svg.Svg Action
noteWave color zoom svgWidth note =
    let
        xDelta =
            (2 * zoom) / svgWidth

        lines =
            note.waveform
                |> Array.toList
                |> dropToLocalMinimum
                |> List.take (round svgWidth)
                |> List.indexedMap (\i v -> L { x = toFloat i * xDelta, y = v })

        initial =
            note.waveform |> Array.get 0 |> Maybe.withDefault 0
    in
    path
        [ fill "none"
        , stroke color
        , strokeOpacity "0.5"
        , strokeWidth "0.03"
        , M { x = 0, y = initial } :: lines |> pathDefinition |> d
        ]
        []


keyboard notes =
    let
        isSharp i =
            [ 1, 3, 6, 8, 10 ] |> Set.fromList |> Set.member i
    in
    div
        [ style "flex" "1"
        , style "display" "flex"
        , style "flex-direction" "column"
        ]
        (colors
            |> List.indexedMap
                (\i color ->
                    div
                        [ style "background-color" color
                        , style "flex" "1"
                        , style "margin" "2px"
                        , style "width"
                            (if isSharp i then
                                "50%"

                             else
                                "auto"
                            )
                        , style "box-shadow"
                            (if Set.member i notes then
                                "unset"

                             else
                                "0px 0px 10vh inset white"
                            )
                        , onClick (ToggleKey i)
                        ]
                        []
                )
            |> List.reverse
        )


parsePoint : (Float -> Float -> a) -> Json.Decoder a
parsePoint tagger =
    Json.map2 tagger
        (Json.field "clientX" Json.float)
        (Json.field "clientY" Json.float)


pointToZoom : (Point -> ZoomAction) -> Float -> Float -> Action
pointToZoom t x y =
    Point x y |> t >> Zoom


parseZoom =
    pointToZoom >> parsePoint


zoomEvents : List (Html.Attribute Action)
zoomEvents =
    [ Html.Events.onMouseLeave (Zoom ZoomStop)
    , Html.Events.onMouseUp (Zoom ZoomStop)
    , Html.Events.on "mousemove" (parseZoom ZoomChange)
    , Html.Events.on "mousedown" (parseZoom ZoomStart)
    ]


view : Model -> Html.Html Action
view model =
    let
        svgWidth =
            model.scene.height

        noteIds =
            model.notes |> Dict.keys |> Set.fromList
    in
    div
        [ style "display" "flex"
        , style "height" (String.fromFloat model.scene.height)
        , style "margin" "20px"
        ]
        [ keyboard noteIds
        , div
            ([ style "flex" "4", style "cursor" "ew-resize" ]
                ++ zoomEvents
            )
            [ svg
                [ width (String.fromFloat svgWidth)
                , height (String.fromFloat (model.scene.height - 40))
                , viewBox "0 -1 2 2"
                ]
                (List.indexedMap
                    (\noteId color ->
                        case Dict.get noteId model.notes of
                            Just note ->
                                noteWave color (model.zoom * 10) svgWidth note

                            Nothing ->
                                path [] []
                    )
                    colors
                )
            ]
        ]
