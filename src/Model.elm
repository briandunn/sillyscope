module Model exposing (Action(..), AudioSource, Model, Point, ViewportAction(..), Waveform, WidthHeight, ZoomAction(..), dropToLocalMinimum, init, micId)

import Browser.Dom exposing (Element, Viewport)
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import OscilatorType exposing (OscilatorType(..))
import Task exposing (attempt, perform)
import Waveform


type alias Waveform =
    Waveform.Waveform


dropToLocalMinimum =
    Waveform.dropToLocalMinimum


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
    | UpdateFfts E.Value
    | AddAudioSource D.Value
    | SetOscilatorType OscilatorType


type alias Analysis =
    { waveform : Waveform, fft : Waveform, dominantFreq : Int }


type alias AudioSource =
    { id : Int, node : D.Value, analysis : Maybe Analysis }


type alias Model =
    { waveforms : Dict Int Waveform
    , dominantFrequencies : Dict Int Int
    , ffts : Dict Int Waveform
    , audioSources : Dict Int AudioSource
    , zoom : Float
    , zoomStart : Maybe Point
    , wrapperElement : Maybe Element
    , oscilatorType : OscilatorType
    }


micId =
    777


type alias Point =
    { x : Float, y : Float }


type alias WidthHeight =
    { width : Int, height : Int }


init : () -> ( Model, Cmd Action )
init () =
    ( { waveforms = Dict.empty
      , audioSources = Dict.empty
      , ffts = Dict.empty
      , dominantFrequencies = Dict.empty
      , zoom = 1
      , zoomStart = Nothing
      , wrapperElement = Nothing
      , oscilatorType = Sine
      }
    , perform (\viewport -> viewport |> ViewportSet |> Viewport) Browser.Dom.getViewport
    )
