module View exposing (view)

import Array exposing (toList)
import Dict
import Html exposing (div)
import Html.Attributes exposing (id, style)
import Html.Events exposing (on, onClick)
import Json.Decode as Json
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (vec3)
import Model exposing (Action(..), Model, Note, Point, ZoomAction(..))
import OscilatorType exposing (OscilatorType(..))
import PathDefinition exposing (PathCommand(..))
import Set
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, height, stroke, viewBox, width)
import WebGL exposing (Mesh, Shader)


colors =
    [ ( 252, 190, 237 ), ( 61, 52, 4 ), ( 55, 94, 7 ), ( 11, 126, 24 ), ( 18, 155, 124 ), ( 27, 121, 180 ), ( 39, 55, 200 ), ( 105, 55, 216 ), ( 172, 75, 229 ), ( 221, 99, 238 ), ( 245, 128, 240 ), ( 250, 159, 234 ), ( 252, 190, 237 ) ]


selectedAttribute test =
    style "box-shadow"
        (if test then
            "unset"

         else
            "0px 0px 10vh inset white"
        )


toCSSColor ( r, g, b ) =
    "rgb(" ++ ([ r, g, b ] |> List.map String.fromFloat |> String.join ",") ++ ")"


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


view : Model -> Html.Html Action
view model =
    let
        noteIds =
            model.notes |> Dict.keys |> Set.fromList

        dims =
            case model.wrapperElement of
                Nothing ->
                    { sceneHeight = 0, scopeWidth = 0 }

                Just element ->
                    { sceneHeight = element.viewport.height - 40, scopeWidth = element.element.width }
    in
    div
        [ style "display" "flex"
        , style "height" (String.fromFloat dims.sceneHeight ++ "px")
        , style "padding" "20px"
        ]
        [ keyboard noteIds
        , div
            ([ style "flex" "4", style "cursor" "ew-resize", id "scope-wrapper", style "height" "100%" ]
                ++ zoomEvents
            )
            [ WebGL.toHtml
                [ width (String.fromFloat dims.scopeWidth)
                , height (String.fromFloat dims.sceneHeight)
                ]
                (entities model)
            ]
        , div
            [ style "flex" "1", style "flex-direction" "column", style "justify-content" "space-evenly", style "display" "flex" ]
            (OscilatorType.all
                |> Dict.toList
                |> List.map
                    (\( name, t ) ->
                        svg
                            [ style "background-color" "#fcbeed"
                            , style "fill" "none"
                            , style "margin" "30px"
                            , style "stroke" "rgb(105, 55, 216)"
                            , style "stroke-opacity" "0.5"
                            , style "stroke-width" "0.2"
                            , selectedAttribute (name == OscilatorType.toString model.oscilatorType)
                            , width "50"
                            , viewBox "0 -1 2 2"
                            , onClick (SetOscilatorType t)
                            ]
                            [ oscilatorIcon t ]
                    )
            )
        ]


type alias VirtexAttributes =
    { i : Float, val : Float }


mesh waveform =
    waveform
        |> List.indexedMap (\i v -> VirtexAttributes (toFloat i) v)
        |> WebGL.triangleStrip


vertexShader =
    [glsl|
    attribute float val;
    attribute float i;
    uniform float samples;

    bool isEven (in float x) {
        return floor(mod(x,2.0)) == 0.0;
    }

    void main () {
        float x = ((i / samples) * 2.0) - 1.0;
        float delta = isEven(i) ? 0.1 : -0.1;

        gl_Position = vec4(x,val + delta, 0.0, 1.0);
    }
|]


fragmentShader =
    [glsl|

    precision mediump float;
    uniform vec3 color;

    void main () {
        gl_FragColor = vec4(color, 1);
    }
|]


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


noteToEntity note color =
    WebGL.entity vertexShader fragmentShader (mesh note.waveform) { color = color, samples = note.waveform |> List.length |> toFloat }


colorToVec ( r, g, b ) =
    vec3 (r / 255.0) (g / 255.0) (b / 255.0)


entities model =
    colors
        |> List.indexedMap (\i c -> ( i, c ))
        |> List.foldr
            (\( i, color ) es ->
                case Dict.get i model.notes of
                    Just note ->
                        noteToEntity note (colorToVec color) :: es

                    Nothing ->
                        es
            )
            []
