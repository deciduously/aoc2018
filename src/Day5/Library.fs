namespace Day5

module util =
  let getInput inputFile =
    System.IO.File.ReadAllText(inputFile)

  let doesReact first second =
    if System.Char.ToUpper first = System.Char.ToUpper second then
      first <> second
    else
      false

  let rec reactString altered result input =
    match input with
    | [] -> if altered then reactString false "" (string result |> List.ofSeq) else result
    | [a] -> if altered then reactString false "" (string result + string a |> List.ofSeq) else result + string a
    | head::next::tail ->
      if doesReact head next then
        reactString true result tail
      else
        reactString altered (result + string head) ([next] @ tail)

module part1 =
  open util

  let execute inputFile =
    getInput inputFile
    |> List.ofSeq
    |> reactString false ""
    |> String.length

module part2 =
  open util
  let cleanInput targetPair input =
    List.filter (fun el -> el <> System.Char.ToUpper targetPair && el <> System.Char.ToLower targetPair) input
  let execute inputFile =
     let input = getInput inputFile |> List.ofSeq
     let allPairs = [ for l in 'a' .. 'z' do yield l ]
     let res = List.map (fun el -> (el, reactString false "" (cleanInput el input))) allPairs
     List.sortBy (fun el -> String.length (snd el)) res
     |> List.head
     |> snd
     |> String.length

module run =
  let sample = "inputs/sample5.txt"
  let real = "inputs/day5.txt"
  let displayResults inputFile =
    part1.execute inputFile |> printfn "Part 1 result: %i"
    part2.execute inputFile |> printfn "Part 2 result: %A"

  let runBoth =
    printfn "Sample:"
    displayResults sample
    printfn "Real:"
    displayResults real