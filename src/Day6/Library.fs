namespace Day6

 module part1 =
  let execute inputFile = 0
  
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
      printfn "Real:"
      displayResults real 