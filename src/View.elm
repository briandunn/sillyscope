module View exposing (view)

import Array exposing (toList)
import Dict
import Html exposing (div)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onClick)
import Json.Decode as Json
import Model exposing (Action(..), Model, Point, Waveform, ZoomAction(..), micId)
import OscilatorType exposing (OscilatorType(..))
import PathDefinition exposing (PathCommand(..))
import Set
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, height, stroke, viewBox, width)


colors =
    [ "#fcbeed", "#fa9fea", "#f580f0", "#dd63ee", "#ac4be5", "#6937d8", "#2737c8", "#1b79b4", "#129b7c", "#0b7e18", "#375e07", "#3d3404", "#fcbeed" ]


noteWave : String -> Float -> Float -> Waveform -> Svg.Svg Action
noteWave color zoom svgWidth values =
    let
        xDelta =
            (2 * zoom) / svgWidth

        lines =
            values |> List.drop 1 |> List.indexedMap (\i v -> L { x = toFloat i * xDelta, y = v * 0.9 })

        initial =
            values |> List.head |> Maybe.withDefault 0
    in
    path
        [ stroke color
        , M { x = 0, y = initial } :: lines |> PathDefinition.toString |> d
        ]
        []


selectedAttribute test =
    style "box-shadow"
        (if test then
            "unset"

         else
            "0px 0px 10vh inset white"
        )


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
                        , selectedAttribute (Set.member i notes)
                        , onClick (ToggleKey i)
                        ]
                        []
                )
            |> List.reverse
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
view model =
    let
        svgWidth =
            model.scene.height

        noteIds =
            model.audioSources |> Dict.keys |> Set.fromList

        micEnabled =
            Dict.member micId model.audioSources
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
                , style "fill" "none"
                , style "stroke-opacity" "0.5"
                , style "stroke-width" "0.03"
                ]
                ((case Dict.get micId model.waveforms of
                    Just mic ->
                        noteWave "black" (model.zoom * 10) svgWidth mic

                    Nothing ->
                        path [] []
                 )
                    :: List.indexedMap
                        (\noteId color ->
                            case Dict.get noteId model.waveforms of
                                Just waveform ->
                                    noteWave color (model.zoom * 10) svgWidth waveform

                                Nothing ->
                                    path [] []
                        )
                        colors
                )
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
