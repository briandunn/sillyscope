module Main exposing (..)

import Json.Decode as Json
import Browser
import Svg exposing (path, svg)
import Html exposing (div)
import Html.Events
import Svg.Attributes exposing (fill, width, height, viewBox, d, stroke, strokeWidth)
import String


type alias Point =
    { x : Float, y : Float }


main =
    Browser.sandbox { init = (Point 0 0), update = update, view = view }


update : Point -> Point -> Point
update msg model =
    Debug.log "model" msg


lineTo x y =
    "L " ++ x ++ "," ++ y ++ "\n"


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


view : Point -> Html.Html Point
view model =
    div [ onMouseOver Point ]
        [ svg [ width "500", height "500", viewBox "0 -1 2 2" ]
            [ path
                [ fill "none"
                , stroke "chartreuse"
                , strokeWidth "0.01"
                , d (pathDefinition (wave (floor model.x) model.y))
                ]
                []
            ]
        ]
