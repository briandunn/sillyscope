module Model exposing (Action(..), AudioSource, Model, Point, ViewportAction(..), Waveform, WidthHeight, ZoomAction(..), dropToLocalMinimum, micId)

import Browser.Dom exposing (Element, Viewport)
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import OscilatorType exposing (OscilatorType)


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
    | AddDominantFreq D.Value
    | SetOscilatorType OscilatorType


type alias AudioSource =
    { id : Int, node : D.Value }


type alias Waveform =
    List Float


type alias Model =
    { waveforms : Dict Int Waveform
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
