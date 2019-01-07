module Model exposing (Action(..), Model, Note, Point, ViewportAction(..), WidthHeight, ZoomAction(..), dropToLocalMinimum)

import Browser.Dom exposing (Viewport)
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import OscilatorType exposing (OscilatorType)


type ViewportAction
    = ViewportChange WidthHeight
    | ViewportSet Viewport


type ZoomAction
    = ZoomStop
    | ZoomStart Point
    | ZoomChange Point


type Action
    = ToggleKey Int
    | Zoom ZoomAction
    | Viewport ViewportAction
    | Waveform E.Value
    | NotePressed D.Value
    | SetOscilatorType OscilatorType
    | AnimationFrame Float


type alias Note =
    { id : Int, frequency : Float, attack : Float, node : D.Value, waveform : List Float, oscilatorType : OscilatorType }


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
