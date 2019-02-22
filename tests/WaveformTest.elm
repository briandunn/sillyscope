module WaveformTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Waveform


suite : Test
suite =
    describe "autoCorrelate"
        ([ ( Waveform.autoCorrelate, "a" ), ( Waveform.autoCorrelateX, "b" ) ]
            |> List.map
                (\( fn, label ) ->
                    test label <|
                        \_ -> Expect.equal [ 2, -1, 0 ] (fn [ 1, -1, 0 ])
                )
        )
