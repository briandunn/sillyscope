module GL exposing (entities)

import Dict exposing (get)
import List exposing (foldr, indexedMap, length)
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Model exposing (AudioSource)
import WebGL exposing (entity, triangleStrip)
import WebGL.Settings.Blend as Blend


noteToEntity waveform color =
    WebGL.entityWith
        [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha ]
        vertexShader
        fragmentShader
        (mesh waveform)
        { color = color, samples = waveform |> length |> toFloat }


colorToVec ( r, g, b ) =
    vec3 (r / 255.0) (g / 255.0) (b / 255.0)


fftToColor ffts =
    (case ffts |> Dict.values |> List.head |> Maybe.withDefault [] of
        r :: g :: b :: _ ->
            vec3 r g b

        _ ->
            vec3 0 0 0
    )
        |> Debug.log "color"


entities colors { audioSources } =
    let
        getColor : Int -> Vec3
        getColor i =
            colors |> List.drop i |> List.head |> Maybe.map colorToVec |> Maybe.withDefault (fftToColor Dict.empty)

        fold : ( Int, AudioSource ) -> List WebGL.Entity -> List WebGL.Entity
        fold ( id, { analysis } ) list =
            case analysis of
                Just { waveform } ->
                    noteToEntity waveform (getColor id) :: list

                Nothing ->
                    list
    in
    audioSources
        |> Dict.toList
        |> List.foldr fold []


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
        float delta = (isEven(i) ? 1.0 : -1.0) * 0.01;

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
