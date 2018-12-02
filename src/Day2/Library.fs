namespace Day2
open Microsoft.FSharp.Collections

module part1 =
  let getBoxIds fileName =
    let lines = System.IO.File.ReadLines(fileName)
    lines |> Seq.toList

  let tickFreq c (freqs: Map<char, int>) =
    match (freqs.TryFind c) with
    | Some x -> freqs.Add(c, x + 1)
    | None -> freqs.Add(c, 1)

  let rec getFreqs (freqs: Map<char, int>) boxId =
    let len = String.length boxId
    if len = 1 then
      tickFreq boxId.[0] freqs
    else
      getFreqs (tickFreq boxId.[0] freqs) boxId.[1..len - 1]

  let countOfFreq num (freqMap: Map<char, int>) =
    freqMap
      |> Map.filter (fun _ v -> v = num)
      |> Map.toList
      |> List.length

  let getTwosAndThrees boxId =
    let freqs = getFreqs (new Map<char, int> (Seq.empty)) boxId
    let twos = countOfFreq 2 freqs
    let threes = countOfFreq 3 freqs
    (twos, threes)

  let rec getTotals twos threes l =
    match l with
    | [] -> (twos, threes)
    | (two, three)::tail ->
      let newTwos = if two > 0 then twos + 1 else twos
      let newThrees = if three > 0 then threes + 1 else threes
      getTotals newTwos newThrees tail

  let execute fileName =
    let boxIds = getBoxIds fileName
    let twosAndThrees = List.map (fun bid -> getTwosAndThrees bid) boxIds
    let (twos, threes) = getTotals 0 0 twosAndThrees
    twos * threes