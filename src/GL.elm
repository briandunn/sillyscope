module GL exposing (entities)

import Dict exposing (get)
import List exposing (foldr, indexedMap, length)
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (vec3)
import WebGL exposing (entity, triangleStrip)
import WebGL.Settings.Blend as Blend


noteToEntity note color =
    WebGL.entityWith [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha ] vertexShader fragmentShader (mesh note.waveform) { color = color, samples = note.waveform |> length |> toFloat }


colorToVec ( r, g, b ) =
    vec3 (r / 255.0) (g / 255.0) (b / 255.0)


entities colors model =
    colors
        |> indexedMap (\i c -> ( i, c ))
        |> foldr
            (\( i, color ) es ->
                case get i model.notes of
                    Just note ->
                        noteToEntity note (colorToVec color) :: es

                    Nothing ->
                        es
            )
            []


type alias VirtexAttributes =
    { i : Float, val : Float }


mesh waveform =
    waveform
        |> indexedMap (\i v -> VirtexAttributes (toFloat i) v)
        |> triangleStrip


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
        float delta = (isEven(i) ? 1.0 : -1.0) * 0.03;

        gl_Position = vec4(x + delta,val + delta, 0.0, 1.0);
    }
|]


fragmentShader =
    [glsl|

    precision mediump float;
    uniform vec3 color;

    void main () {
        gl_FragColor = vec4(color, 0.85);
    }
|]
