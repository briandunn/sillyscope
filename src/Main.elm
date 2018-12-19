module Main exposing (..)

import Json.Decode as Json
import Browser
import Svg exposing (path, svg)
import Html exposing (div)
import Html.Attributes exposing (style)
import Svg.Attributes exposing (fill, width, height, viewBox, d, stroke, strokeWidth)
import String
import Html.Events exposing (onClick, on)
import Set exposing (Set)


type alias Point =
    { x : Float, y : Float }


type alias Model =
    Set Int


type Action
    = ToggleKey Int


main =
    Browser.sandbox { init = Set.singleton 0, update = update, view = view }


update : Action -> Model -> Model
update action model =
    case action of
        ToggleKey i ->
            if Set.member i model then
                Set.remove i model
            else
                Set.insert i model


type PathCommand
    = M Float Float
    | L Float Float


commandToString : PathCommand -> String
commandToString command =
    case command of
        M x y ->
            "M" ++ String.fromFloat x ++ "," ++ String.fromFloat y

        L x y ->
            "L" ++ String.fromFloat x ++ "," ++ String.fromFloat y


pathDefinition : List PathCommand -> String
pathDefinition commands =
    commands |> List.map commandToString |> String.join " "


wave : Int -> Float -> List PathCommand
wave samples cycles =
    (M 0 0)
        :: (List.range 0 samples
                |> List.map toFloat
                |> List.map
                    (\i ->
                        (L
                            (2 * (i / (toFloat samples)))
                            (sin (cycles * 2 * i * pi / (toFloat samples)))
                        )
                    )
           )


parsePoint : Json.Decoder Point
parsePoint =
    Json.map2 Point
        (Json.field "clientX" Json.float)
        (Json.field "clientY" Json.float)


onMouseOver point =
    Html.Events.on "mouseover" parsePoint


interval : Int -> Float
interval n =
    Debug.log "freq" (2 ^ ((toFloat n) / 12.0))


colors =
    [ "#fcbeed", "#fa9fea", "#f580f0", "#dd63ee", "#ac4be5", "#6937d8", "#2737c8", "#1b79b4", "#129b7c", "#0b7e18", "#375e07", "#3d3404", "#fcbeed" ]


noteWave note color =
    path
        [ fill "none"
        , stroke color
        , strokeWidth "0.01"
        , d (pathDefinition (wave 500 (3 * (interval note))))
        ]
        []



-- (List.map2 noteWave (Set.toList model) colors)


view : Model -> Html.Html Action
view model =
    div [ style "display" "flex" ]
        [ div
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
                            , onClick (ToggleKey i)
                            ]
                            []
                    )
            )
        , div
            [ style "flex" "4" ]
            [ svg [ width "500", height "500", viewBox "0 -1 2 2" ]
                (List.indexedMap
                    (\note color ->
                        if Set.member note model then
                            noteWave note color
                        else
                            path [] []
                    )
                    colors
                )
            ]
        ]
