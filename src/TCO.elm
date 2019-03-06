module TCO exposing (countTo, countToX)

import Benchmark exposing (Benchmark, benchmark, compare, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)


countToX limit =
    let
        countToHelper cur =
            if cur < limit then
                let
                    incremented =
                        Debug.log "callin" (cur + 1)
                in
                incremented :: countToHelper incremented

            else
                []
    in
    countToHelper 0


countTo limit =
    let
        countToHelper acc cur =
            if cur < limit then
                let
                    incremented =
                        Debug.log "callin" (cur + 1)
                in
                countToHelper (acc ++ [ incremented ]) incremented

            else
                acc
    in
    countToHelper [] 0


suite =
    describe "countTo" <| [ benchmark "tco" (\_ -> countTo 500), benchmark "notco" (\_ -> countToX 500) ]


main =
    program suite
