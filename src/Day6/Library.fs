namespace Day6

module util =
  let scrapeCoords inputFile =
    System.IO.File.ReadLines(inputFile)
    |> Seq.mapi (fun i el ->
      let coords = el.Split ','
      (i, (coords.[0] |> System.Convert.ToInt32, coords.[1] |> System.Convert.ToInt32)))
    |> List.ofSeq

  type Coord = (int32 * int32)

  let manhattanDistance (origin: Coord) target =
  // abs of difference in xs + abs of distance in ys
    System.Math.Abs (fst origin - fst target) + System.Math.Abs (snd origin - snd target)

module part1 =
  open util
  let execute inputFile =
    scrapeCoords inputFile |> printfn "%A"
    0
  
module part2 =
  let execute inputFile = 0
  
module run =
  let sample = "inputs/sample6.txt"
  let real = "inputs/real6.txt"
  
  let displayResults inputFile =
    part1.execute inputFile |> printfn "Part 1 result: %i"
    part2.execute inputFile |> printfn "Part 2 result: %i"
    
  let runBoth =
    printfn "Sample:"
    displayResults sample
    //printfn "Real:"
    //displayResults real 