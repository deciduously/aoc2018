open System
open Day1

[<EntryPoint>]
let main argv =
    if Array.length argv = 0 then
        printfn "Provide a day: day1, day2, etc"
    else 
        match argv.[0] with
            | "day1" ->
                let inputFile = "..\..\inputs/day1.txt"
                day1Part1 inputFile |> printfn "Part 1 result: %i"
                day1Part2 inputFile |> printfn "Part 2 result: %i"
            | _ -> printfn "Unknown day"

    0 // return an integer exit code