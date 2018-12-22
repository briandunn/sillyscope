module Main exposing (Action(..), Model, PathCommand(..), colors, commandToString, interval, main, noteWave, pathDefinition, update, view, wave)

import Browser
import Browser.Dom
import Html exposing (div, input)
import Html.Attributes exposing (style, type_)
import Html.Events exposing (on, onClick)
import Json.Decode as Json
import Set exposing (Set)
import String
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, fill, height, stroke, strokeOpacity, strokeWidth, viewBox, width)
import Task


type alias Model =
    { notes : Set Int
    , zoom : Float
    , zoomStart : Maybe Point
    , scene : { width : Float, height : Float }
    }


type alias Point =
    { x : Float, y : Float }


type ZoomAction
    = ZoomStop
    | ZoomStart Point
    | ZoomChange Point


type Action
    = ToggleKey Int
    | Zoom ZoomAction
    | ViewportChange Browser.Dom.Viewport


init () =
    ( { notes = Set.singleton 0, zoom = 0.5, zoomStart = Nothing, scene = { width = 1, height = 1 } }
    , Task.perform ViewportChange Browser.Dom.getViewport
    )


subscriptions model =
    Sub.none


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


toggle element set =
    if Set.member element set then
        Set.remove element set

    else
        Set.insert element set


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    ( case action of
        ToggleKey i ->
            { model | notes = toggle i model.notes }

        ViewportChange viewPort ->
            { model | scene = viewPort.scene }

        Zoom zoom ->
            case zoom of
                ZoomStart point ->
                    { model | zoomStart = Just point }

                ZoomChange point ->
                    case model.zoomStart of
                        Nothing ->
                            model

                        Just start ->
                            { model | zoom = (point.x - start.x) / model.scene.width }

                ZoomStop ->
                    { model | zoomStart = Nothing }
    , Cmd.none
    )


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


wave : Float -> Float -> List PathCommand
wave samples cycles =
    M { x = 0, y = 0 }
        :: (List.range 0 (round samples)
                |> List.map toFloat
                |> List.map
                    (\i ->
                        L
                            { x = 2 * (i / samples)
                            , y =
                                if sin (cycles * 2 * i * pi / samples) < 0 then
                                    -1.0

                                else
                                    1
                            }
                    )
           )


interval : Int -> Float
interval n =
    2 ^ (toFloat n / 12.0)


colors =
    [ "#fcbeed", "#fa9fea", "#f580f0", "#dd63ee", "#ac4be5", "#6937d8", "#2737c8", "#1b79b4", "#129b7c", "#0b7e18", "#375e07", "#3d3404", "#fcbeed" ]


noteWave note color zoom svgWidth =
    path
        [ fill "none"
        , stroke color
        , strokeOpacity "0.5"
        , strokeWidth "0.03"
        , d (pathDefinition (wave svgWidth (zoom * interval note)))
        ]
        []


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
            |> List.reverse
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
    let
        svgWidth =
            model.scene.height
    in
    div [ style "display" "flex" ]
        [ keyboard model.notes
        , div
            ([ style "flex" "4", style "cursor" "ew-resize" ]
                ++ zoomEvents
            )
            [ svg [ width (String.fromFloat svgWidth), height (String.fromFloat model.scene.height), viewBox "0 -1 2 2" ]
                (List.indexedMap
                    (\note color ->
                        if Set.member note model.notes then
                            noteWave note color (model.zoom * 10) svgWidth

                        else
                            path [] []
                    )
                    colors
                )
            ]
        ]
