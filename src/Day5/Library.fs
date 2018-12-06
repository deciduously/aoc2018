namespace Day5

module util =
  let getInput inputFile =
    System.IO.File.ReadAllText(inputFile) |> List.ofSeq

  let doesReact first second =
    (System.Char.ToUpper first = System.Char.ToUpper second) && first <> second

  // unused and very slow - left for posterity
  let rec reactString altered result input =
    match input with
    | [] -> if altered then reactString false "" (string result |> List.ofSeq) else result
    | [a] -> if altered then reactString false "" (string result + string a |> List.ofSeq) else result + string a
    | head::next::tail ->
      if doesReact head next then
        reactString true result tail
      else
        reactString altered (result + string head) ([next] @ tail)

  let reactQuickly input =
    Seq.fold (fun s c ->
      let last = if Array.length s > 0 then Some (Array.last s) else None
      match last with
      | Some x ->
        if c <> x && (x = System.Char.ToUpper c || x = System.Char.ToLower c) then
          Array.sub s 0 (Array.length s - 1)
        else Array.append s [| c |]
      | None -> Array.append s [| c |]) [| |] input
        |> Array.length

module part1redo =
  open util
  let execute inputFile =
    getInput inputFile |> reactQuickly

module part2redo =
  open util
  
  let cleanInput targetPair input =
    List.filter (fun el -> el <> System.Char.ToUpper targetPair && el <> System.Char.ToLower targetPair) input

  let execute inputFile =
     let input = getInput inputFile
     let allPairs = [ for l in 'a' .. 'z' do yield l ]
     List.map (fun el -> (el, reactQuickly (cleanInput el input))) allPairs
     |> List.sortBy (fun el -> snd el)
     |> List.head
     |> snd

module runRedo =
  let sample = "inputs/sample5.txt"
  let real = "inputs/day5.txt"
  let displayResultsRedo inputFile =
    part1redo.execute inputFile |> printfn "Part 1 result: %i"
    part2redo.execute inputFile |> printfn "Part 2 result: %A"

  let runBoth =
    printfn "Running redo"
    printfn "Sample:"
    displayResultsRedo sample
    printfn "Real:"
    displayResultsRedo real
