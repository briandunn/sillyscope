module PathDefinition exposing (PathCommand(..), toString)


type alias Point =
    { x : Float, y : Float }


type PathCommand
    = M Point
    | L Point
    | Q Point Point (List Point)
    | C Point Point Point (List Point)


commandToString : PathCommand -> String
commandToString command =
    let
        pointToString p =
            String.join "," (List.map String.fromFloat [ p.x, p.y ])
    in
    case command of
        M p ->
            "M " ++ pointToString p

        L p ->
            "L " ++ pointToString p

        Q p1 p2 l ->
            "Q " ++ (p1 :: p2 :: l |> List.map pointToString |> String.join " ")

        C p1 p2 p3 l ->
            "Q " ++ (p1 :: p2 :: p3 :: l |> List.map pointToString |> String.join " ")


toString : List PathCommand -> String
toString commands =
    commands |> List.map commandToString |> String.join " "
