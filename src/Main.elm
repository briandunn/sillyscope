module Main exposing (..)

import Browser
import Svg exposing (path, svg)
import Svg.Attributes exposing (fill, width, height, viewBox, d, stroke, strokeWidth)
import String


main =
    Browser.sandbox { init = 0, update = update, view = view }


update msg model =
    model


type alias Model =
    {}


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
                        (L (i / (toFloat samples)) (sin (cycles * i * pi / (toFloat samples))))
                    )
           )


view msg =
    svg [ width "500", height "500", viewBox "0 -1 1 2" ]
        [ path
            [ fill "none", stroke "chartreuse", strokeWidth "0.01", d (pathDefinition (wave 10 9)) ]
            []
        ]
