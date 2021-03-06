module View exposing (view)

import Array exposing (toList)
import Dict exposing (Dict)
import GL
import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (id, style)
import Html.Events exposing (on, onClick)
import Json.Decode as Json
import Model exposing (Action(..), AudioSource, Model, Point, ZoomAction(..), freqToNoteId, micId)
import OscilatorType exposing (OscilatorType(..))
import PathDefinition exposing (PathCommand(..))
import Set exposing (Set)
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, height, viewBox, width)
import Waveform
import WebGL


frameCount { wrapperElement, zoom } =
    case wrapperElement of
        Nothing ->
            0

        Just element ->
            round (element.element.width * zoom)


trim model =
    Waveform.dropToLocalMinimum >> List.take (frameCount model)


trimWaveforms : Model -> Model
trimWaveforms model =
    let
        updateSource _ source =
            { source
                | waveform = Maybe.map (trim model) source.waveform
                , frequency = Maybe.map freqToNoteId source.frequency
            }
    in
    { model
        | audioSources =
            model.audioSources |> Dict.map updateSource
    }


colors : List (number, number, number)
colors =
    [ ( 252, 190, 237 ), ( 61, 52, 4 ), ( 55, 94, 7 ), ( 11, 126, 24 ), ( 18, 155, 124 ), ( 27, 121, 180 ), ( 39, 55, 200 ), ( 105, 55, 216 ), ( 172, 75, 229 ), ( 221, 99, 238 ), ( 245, 128, 240 ), ( 250, 159, 234 ), ( 252, 190, 237 ) ]
        |> List.reverse


selectedAttribute : Bool -> Attribute msg
selectedAttribute test =
    style "box-shadow"
        (if test then
            "unset"

         else
            "0px 0px 10vh inset white"
        )


toCSSColor ( r, g, b ) =
    "rgb(" ++ ([ r, g, b ] |> List.map String.fromFloat |> String.join ",") ++ ")"


needle note cent =
    let
        toPercent f =
            String.fromFloat (f * 100) ++ "%"
    in
    div
        [ style "position" "absolute"
        , style "background-color" "rgba(0,0,0,0.3)"
        , style "height" "1%"
        , style "top" (toPercent note)
        , style "transition" "top 0.3s"
        , style "width" "100%"
        ]
        [ div
            [ style "position" "absolute"
            , style "width" "5%"
            , style "transition" "left 0.3s"
            , style "left" (toPercent cent)
            , style "height" "100%"
            , style "background-color" purple
            ]
            []
        ]


tunerNeedle keyCount freq =
    let
        keys =
            toFloat keyCount

        note =
            ((keys - freq) / keys) - (0.5 / keys)

        cent =
            (freq - toFloat (round freq)) + 0.5
    in
    needle note cent


keyboard : Dict Int AudioSource -> Html Action
keyboard sources =
    let
        notes =
            sources |> Dict.keys |> Set.fromList

        isSharp i =
            [ 1, 3, 6, 8, 10 ] |> Set.fromList |> Set.member i
    in
    div
        [ style "flex" "1"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "position" "relative"
        ]
        ((sources
            |> Dict.values
            |> List.filterMap .frequency
            |> List.filter ((<) 0)
            |> List.map (tunerNeedle (List.length colors))
         )
            ++ (colors
                    |> List.indexedMap
                        (\i color ->
                            div
                                [ color |> toCSSColor |> style "background-color"
                                , style "flex" "1"
                                , style "margin" "2px"
                                , style "width"
                                    (if isSharp i then
                                        "50%"

                                     else
                                        "auto"
                                    )
                                , selectedAttribute (Set.member i notes)
                                , onClick (ToggleKey i)
                                ]
                                []
                        )
                    |> List.reverse
               )
        )


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
                |> PathDefinition.toString
    in
    path [ d pathDef ] []


purple =
    "rgb(105, 55, 216)"


buttonSvg : Bool -> List (Attribute Action) -> List (Html Action) -> Html Action
buttonSvg selected attrs children =
    svg
        ([ style "background-color" "#fcbeed"
         , style "fill" "none"
         , style "margin" "30px"
         , style "stroke" purple
         , style "stroke-opacity" "0.5"
         , style "stroke-width" "0.2"
         , selectedAttribute selected
         , width "50"
         ]
            ++ attrs
        )
        children


view : Model -> Html.Html Action
view model_ =
    let
        model =
            trimWaveforms model_

        dims =
            case model.wrapperElement of
                Nothing ->
                    { sceneHeight = 0, scopeWidth = 0 }

                Just element ->
                    { sceneHeight = element.viewport.height - 40, scopeWidth = element.element.width }

        micEnabled =
            Dict.member micId model.audioSources
    in
    div
        [ style "display" "flex"
        , style "height" (String.fromFloat dims.sceneHeight ++ "px")
        , style "padding" "20px"
        ]
        [ keyboard model.audioSources
        , div
            ([ style "flex" "4", style "cursor" "ew-resize", id "scope-wrapper", style "height" "100%", style "position" "relative" ]
                ++ zoomEvents
            )
            [ WebGL.toHtml
                [ width (String.fromFloat dims.scopeWidth)
                , height (String.fromFloat dims.sceneHeight)
                ]
                (GL.entities colors model)
            ]
        , div
            [ style "flex" "1", style "flex-direction" "column", style "justify-content" "space-evenly", style "display" "flex" ]
            (buttonSvg
                micEnabled
                [ onClick ToggleMic
                , viewBox "0 0 22 22"
                , style "fill" purple
                , style "fill-opacity" "0.5"
                ]
                [ path
                    [ d "M 11,8 C 9.892,8 9,8.892 9,10 l 0,3 c 0,1.108 0.892,2 2,2 1.108,0 2,-0.892 2,-2 l 0,-3 C 13,8.892 12.108,8 11,8 Z" ]
                    []
                , path [ d "m 7,12 0,1 c 0,1.8474 1.2856599,3.4048 3,3.8555 L 10,18 l -1,0 0,1 4,0 0,-1 -1,0 0,-1.1445 C 13.71434,16.4048 15,14.8474 15,13 l 0,-1 -1,0 0,1 c 0,1.6447 -1.355297,3 -3,3 -1.6447028,0 -3,-1.3553 -3,-3 l 0,-1 -1,0 z" ]
                    []
                ]
                :: (OscilatorType.all
                        |> Dict.toList
                        |> List.map
                            (\( name, t ) ->
                                buttonSvg (name == OscilatorType.toString model.oscilatorType)
                                    [ onClick (SetOscilatorType t)
                                    , viewBox "0 -1 2 2"
                                    ]
                                    [ oscilatorIcon t ]
                            )
                   )
            )
        ]


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
