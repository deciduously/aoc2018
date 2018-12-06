[<EntryPoint>]
let main argv =
  if Array.length argv = 0 then
    printfn "Provide a day: day1, day2, etc"
  else 
    match argv.[0] with
    | "day1" ->
     printfn "Day 1:"
     Day1.run.runBoth
    | "day2" ->
     printfn "Day 2: "
     Day2.run.runBoth
    | "day3" ->
     printfn "Day 3:"
     Day3.run.runBoth
    | "day4" ->
     printfn "Day 4:"
     Day4.run.runBoth
    | "day5" ->
     printfn "Day 5:"
     Day5.runRedo.runBoth
    | "day6" ->
     printfn "Day 6:"
     Day6.run.runBoth
    | _ -> printfn "Unknown day"

  0 // return an integer exit code