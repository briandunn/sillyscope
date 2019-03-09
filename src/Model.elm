module Model exposing (Action(..), AudioSource, Model, Point, ViewportAction(..), Waveform, WidthHeight, ZoomAction(..), freq, freqToNoteId, init, micId, note, noteIdToFreq, updateAudioSources)

import Browser.Dom exposing (Element, Viewport)
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import OscilatorType exposing (OscilatorType(..))
import Task exposing (attempt, perform)


type alias Waveform =
    List Float


type ViewportAction
    = ViewportChange WidthHeight
    | ViewportSet Viewport
    | WrapperElement (Result Browser.Dom.Error Element)


type ZoomAction
    = ZoomStop
    | ZoomStart Point
    | ZoomChange Point


type Action
    = ToggleKey Int
    | ToggleMic
    | Zoom ZoomAction
    | Viewport ViewportAction
    | UpdateWaveform E.Value
    | UpdateFrequency E.Value
    | AddAudioSource D.Value
    | SetOscilatorType OscilatorType


type alias AudioSource =
    { id : Int, node : D.Value, waveform : Maybe Waveform, frequency : Maybe Float }


type alias Model =
    { audioSources : Dict Int AudioSource
    , zoom : Float
    , zoomStart : Maybe Point
    , wrapperElement : Maybe Element
    , oscilatorType : OscilatorType
    , sampleRate : Int
    }


micId =
    777


noteIdToFreq : Int -> Float
noteIdToFreq =
    toFloat >> (+) 40 >> freq


freqToNoteId f =
    let
        n =
            note f - 4
    in
    n - toFloat (12 * floor (n / 12))


freq n =
    440 * (2 ^ (1 / 12)) ^ (n - 49)


note f =
    logBase (2 ^ (1 / 12)) (f / 440) + 49


type alias Point =
    { x : Float, y : Float }


type alias WidthHeight =
    { width : Int, height : Int }


init : Int -> ( Model, Cmd Action )
init sampleRate =
    ( { audioSources = Dict.empty
      , zoom = 1
      , sampleRate = sampleRate
      , zoomStart = Nothing
      , wrapperElement = Nothing
      , oscilatorType = Sine
      }
    , perform (\viewport -> viewport |> ViewportSet |> Viewport) Browser.Dom.getViewport
    )


updateAudioSources : (AudioSource -> b -> AudioSource) -> Dict Int AudioSource -> Dict Int b -> Dict Int AudioSource
updateAudioSources fn sources data =
    let
        skip _ _ r =
            r

        both id s d r =
            Dict.insert id (fn d s) r
    in
    Dict.merge skip both skip data sources sources
