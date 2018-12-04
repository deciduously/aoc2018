open System

[<EntryPoint>]
let main argv =
  if Array.length argv = 0 then
    printfn "Provide a day: day1, day2, etc"
  else 
    match argv.[0] with
    | "day1" ->
      printfn "Day 1:"
      let inputFile = "../../inputs/day1.txt"
      Day1.part1.execute inputFile |> printfn "Part 1 result: %i"
      Day1.part2.execute inputFile |> printfn "Part 2 result: %i"
    | "day2" ->
      printfn "Day 2: "
      let inputFile = "../../inputs/day2.txt"
      Day2.part1.execute inputFile |> printfn "Part 1 result: %i"
      Day2.part2.execute inputFile |> printfn "Part 2 result: %A"
    | "day3" ->
      printfn "Day 3:"
      let inputFile = "../../inputs/day3.txt"
      Day3.part1.execute inputFile |> printfn "Part 1 result: %i"
      Day3.part2.execute inputFile |> printfn "Part 2 result: %i"
    | "day4" ->
      let inputFile = "../../inputs/day4.txt"
      Day4.part1.execute inputFile |> printfn "Part 1 result: %i"
      Day4.part2.execute inputFile |> printfn "Part 2 result: %i"
    | _ -> printfn "Unknown day"

  0 // return an integer exit code