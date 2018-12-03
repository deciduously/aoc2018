namespace Day2
open Microsoft.FSharp.Collections

module util =
    type FreqMap = Map<char, int>

    let getBoxIds fileName =
      let lines = System.IO.File.ReadLines(fileName)
      lines |> Seq.toList

    let tickFreq c (freqs: FreqMap) =
      match (freqs.TryFind c) with
      | Some x -> Map.add c (x + 1) freqs
      | None -> Map.add c 1 freqs

    let rec goGetFreqs (freqs: FreqMap) boxId =
      let len = String.length boxId
      if len = 1 then
        tickFreq boxId.[0] freqs
      else
        goGetFreqs (tickFreq boxId.[0] freqs) boxId.[1..len - 1]

    let getFreqs boxId = goGetFreqs (new Map<char, int> (Seq.empty)) boxId

    let countOfFreq num (freqMap: FreqMap) =
      freqMap
        |> Map.filter (fun _ v -> v = num)
        |> Map.toList
        |> List.length

module part1 =
  let getTwosAndThrees boxId =
    let freqs = util.getFreqs boxId
    let twos = util.countOfFreq 2 freqs
    let threes = util.countOfFreq 3 freqs
    (twos, threes)

  let rec getTotals twos threes l =
    match l with
    | [] -> (twos, threes)
    | (two, three)::tail ->
      let newTwos = if two > 0 then twos + 1 else twos
      let newThrees = if three > 0 then threes + 1 else threes
      getTotals newTwos newThrees tail

  let execute fileName =
    let boxIds = util.getBoxIds fileName
    let twosAndThrees = List.map (fun bid -> getTwosAndThrees bid) boxIds
    let (twos, threes) = getTotals 0 0 twosAndThrees
    twos * threes

module part2 =
  let checkTwo id id' =
    let s = (Seq.map2 (fun c c' -> 
      if c <> c' then
        '!'
      else
        c) id id')
          |> Seq.filter (fun c -> c <> '!')
          |> Seq.toArray
          |> System.String

    if (String.length id) - 1 = String.length s then
      Some s
    else
      None

  let findWinner boxIds =
    List.map (fun id ->
      List.map (fun id' ->
        if id = id' then
          None
        else
          checkTwo id id') boxIds) boxIds
            |> List.map (List.filter (fun el -> el.IsSome))
            |> List.filter (fun el -> List.length el > 0)
            |> List.concat
            |> List.distinct

  let execute fileName =
    let boxIds = util.getBoxIds fileName
    findWinner boxIds
    