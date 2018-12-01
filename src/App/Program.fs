open System
open Library

[<EntryPoint>]
let main argv =
    day1Part1 argv.[0] |> printfn "Part 1 result: %i"
    day1Part2 argv.[0] |> printfn "Part 2 result: %i"

    0 // return an integer exit code