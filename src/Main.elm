module Main exposing (Action(..), Model, PathCommand(..), colors, commandToString, interval, main, noteWave, pathDefinition, update, view, wave)

import Browser
import Html exposing (div, input)
import Html.Attributes exposing (style, type_)
import Html.Events exposing (on, onClick)
import Json.Decode as Json
import Set exposing (Set)
import String
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, fill, height, stroke, strokeWidth, viewBox, width)


type alias Model =
    { notes : Set Int, zoom : Float, zoomStart : Maybe Point }


type alias Point =
    { x : Float, y : Float }


type ZoomAction
    = ZoomStop
    | ZoomStart Point
    | ZoomChange Point


type Action
    = ToggleKey Int
    | Zoom ZoomAction


main =
    Browser.sandbox
        { init = { notes = Set.singleton 0, zoom = 1, zoomStart = Nothing }
        , update = update
        , view = view
        }


toggle element set =
    if Set.member element set then
        Set.remove element set

    else
        Set.insert element set


update : Action -> Model -> Model
update action model =
    case action of
        ToggleKey i ->
            { model | notes = toggle i model.notes }

        Zoom zoom ->
            case zoom of
                ZoomStart point ->
                    { model | zoomStart = Just point }

                ZoomChange point ->
                    case model.zoomStart of
                        Nothing ->
                            model

                        Just start ->
                            { model | zoom = abs (point.x - start.x) / 500.0 }

                ZoomStop ->
                    { model | zoomStart = Nothing }


type PathCommand
    = M Point
    | L Point


commandToString : PathCommand -> String
commandToString command =
    let
        toString c p =
            c ++ String.join "," (List.map String.fromFloat [ p.x, p.y ])
    in
    case command of
        M p ->
            toString "M" p

        L p ->
            toString "L" p


pathDefinition : List PathCommand -> String
pathDefinition commands =
    commands |> List.map commandToString |> String.join " "


wave : Int -> Float -> List PathCommand
wave samples cycles =
    M { x = 0, y = 0 }
        :: (List.range 0 samples
                |> List.map toFloat
                |> List.map
                    (\i ->
                        L
                            { x = 2 * (i / toFloat samples)
                            , y = sin (cycles * 2 * i * pi / toFloat samples)
                            }
                    )
           )


interval : Int -> Float
interval n =
    2 ^ (toFloat n / 12.0)


colors =
    [ "#fcbeed", "#fa9fea", "#f580f0", "#dd63ee", "#ac4be5", "#6937d8", "#2737c8", "#1b79b4", "#129b7c", "#0b7e18", "#375e07", "#3d3404", "#fcbeed" ]


noteWave note color zoom =
    path
        [ fill "none"
        , stroke color
        , strokeWidth "0.03"
        , d (pathDefinition (wave 500 (zoom * interval note)))
        ]
        []


keyboard notes =
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
                        , style "box-shadow"
                            (if Set.member i notes then
                                "unset"

                             else
                                "0px 0px 10vh inset white"
                            )
                        , onClick (ToggleKey i)
                        ]
                        []
                )
        )


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


view : Model -> Html.Html Action
view model =
    div [ style "display" "flex" ]
        [ keyboard model.notes
        , div
            ([ style "flex" "4", style "cursor" "ew-resize" ]
                ++ zoomEvents
            )
            [ svg [ width "500", height "500", viewBox "0 -1 2 2" ]
                (List.indexedMap
                    (\note color ->
                        if Set.member note model.notes then
                            noteWave note color (model.zoom * 5 + 1)

                        else
                            path [] []
                    )
                    colors
                )
            ]
        ]
