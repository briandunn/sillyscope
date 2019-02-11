module Model exposing (Action(..), Analysis, AudioSource, Model, Point, ViewportAction(..), Waveform, WidthHeight, ZoomAction(..), init, micId)

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
    | AddAudioSource D.Value
    | SetOscilatorType OscilatorType


type alias Analysis =
    { waveform : Waveform, frequency : Float }


type alias AudioSource =
    { id : Int, node : D.Value, analysis : Maybe Analysis }


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
