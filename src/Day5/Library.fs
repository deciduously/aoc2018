namespace Day5

module part1 =
  let execute inputeFile = 0

module part2 =
  let execute inputFile = 0

module run =
  let sample = "inputs/sample5.txt"
  let real = "inputs/real5.txt"
  let displayResults inputFile =
    part1.execute inputFile |> printfn "Part 1 result: %i"
    part2.execute inputFile |> printfn "Part 2 result: %i"

  let runBoth =
    printfn "Sample:"
    displayResults sample
    printfn "Real:"
    displayResults real