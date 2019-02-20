module Bench exposing (main)

import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Waveform exposing (detectFrequency)


suite : Benchmark
suite =
    let
        sampleRate =
            44100

        size =
            2 ^ 8

        freq =
            440

        inc =
            (2 * pi) * (freq / sampleRate)

        sinWave count =
            if count < size then
                sin (count * inc) :: sinWave (count + 1)

            else
                []

        sinSignal =
            sinWave 0
    in
    describe "detectFrequency"
        [ benchmark "440" <| \_ -> detectFrequency sampleRate sinSignal
        ]


main : BenchmarkProgram
main =
    program suite
