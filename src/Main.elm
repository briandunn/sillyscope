port module Main exposing (Action(..), Model, PathCommand(..), colors, commandToString, interval, main, noteWave, pathDefinition, update, view)

import Array exposing (Array)
import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Html exposing (div, input, text)
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
    { id : Int, frequency : Float, attack : Float, node : Json.Value, waveform : Array Float, oscilatorType : OscilatorType }


type OscilatorType
    = Sine
    | Square
    | Sawtooth
    | Triangle


type alias Model =
    { notes : Dict Int Note
    , zoom : Float
    , zoomStart : Maybe Point
    , scene : { width : Float, height : Float }
    , oscilatorType : OscilatorType
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
    | SetOscilatorType OscilatorType


port noteRelease : Json.Value -> Cmd msg


port notePress : Json.Value -> Cmd msg


port notePressed : (Json.Encode.Value -> msg) -> Sub msg


port waveform : (Json.Encode.Value -> msg) -> Sub msg


init : () -> ( Model, Cmd Action )
init () =
    ( { notes = Dict.empty, zoom = 0.25, zoomStart = Nothing, scene = { width = 1, height = 1 }, oscilatorType = Sine }
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


oscilatorTypes =
    [ ( "sawtooth", Sawtooth ), ( "sine", Sine ), ( "square", Square ), ( "triange", Triangle ) ] |> Dict.fromList


oscilatorTypeToString ot =
    case ot of
        Sawtooth ->
            "sawtooth"

        Sine ->
            "sine"

        Square ->
            "square"

        Triangle ->
            "triangle"


stringToOscliatorType s =
    oscilatorTypes |> Dict.get s |> Maybe.withDefault Sine


buildNoteCommand n ot =
    let
        -- middle c
        freq =
            (n + 3) |> interval |> (*) 220

        oscilatorType =
            oscilatorTypeToString ot

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
            Json.map6 Note (Json.field "id" Json.int) (Json.field "frequency" Json.float) (Json.field "attack" Json.float) (Json.field "node" Json.value) (Json.succeed Array.empty) (Json.succeed Sine)
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
                    ( model, buildNoteCommand i model.oscilatorType )

        SetOscilatorType oscilatorType ->
            ( { model | oscilatorType = oscilatorType }, Cmd.none )

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
    | Q Point Point (List Point)
    | C Point Point Point (List Point)


commandToString : PathCommand -> String
commandToString command =
    let
        toString p =
            String.join "," (List.map String.fromFloat [ p.x, p.y ])
    in
    case command of
        M p ->
            "M " ++ toString p

        L p ->
            "L " ++ toString p

        Q p1 p2 l ->
            "Q " ++ (p1 :: p2 :: l |> List.map toString |> String.join " ")

        C p1 p2 p3 l ->
            "Q " ++ (p1 :: p2 :: p3 :: l |> List.map toString |> String.join " ")


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
        |> dropWhileFirstTwo (\a b -> (a > 0 && b > 0) || (a <= 0 && b <= 0))
        |> List.drop 1
        |> dropWhileFirstTwo (\a b -> a > 0 && b > 0)


noteWave : String -> Float -> Float -> Note -> Svg.Svg Action
noteWave color zoom svgWidth note =
    let
        xDelta =
            (2 * zoom) / svgWidth

        values =
            note.waveform
                |> Array.toList
                |> dropToLocalMinimum
                |> List.take (round svgWidth)

        lines = values |> List.drop 1 |> List.indexedMap (\i v -> L { x = toFloat i * xDelta, y = v })

        initial =
            values |> List.head |> Maybe.withDefault 0
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


oscilatorIcon t =
    let
        pathDef =
            M { x = 0, y = 0 }
                :: (case t of
                        Square ->
                            [ M { x = 0, y = -0.75 }, L { x = 1, y = -0.75 }, L { x = 1, y = 0.75 }, L { x = 2, y = 0.75 } ]

                        Sawtooth ->
                            [ M { x = 0, y = -0.75 }, L { x = 1, y = 0.75 }, L { x = 1, y = -0.75 }, L { x = 2, y = 0.75 } ]

                        Triangle ->
                            [ L { x = 0.5, y = -0.75 }, L { x = 1.5, y = 0.75 }, L { x = 2, y = 0 } ]

                        _ ->
                            [ Q { x = 0.5, y = -1.5 } { x = 1, y = 0 } []
                            , Q { x = 1.5, y = 1.5 } { x = 2, y = 0 } []
                            ]
                   )
                |> pathDefinition
    in
    path [ stroke "#fcbeed", strokeWidth "0.2", fill "none", d pathDef ] []


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
        , div
            [ style "flex" "1", style "flex-direction" "column", style "display" "flex" ]
            (oscilatorTypes
                |> Dict.toList
                |> List.map (\( name, t ) -> svg [ style "margin" "30px", style "border" "3px solid #fcbeed", width "30", height "30", viewBox "0 -1 2 2", onClick (SetOscilatorType t) ] [ oscilatorIcon t ])
            )
        ]
