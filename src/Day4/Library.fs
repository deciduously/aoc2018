namespace Day4
open System.Text.RegularExpressions

module util =
  let getContents fileName =
    System.IO.File.ReadLines(fileName) |> Seq.toList

  type LogEntry =
    | NewGuard of System.DateTime * int
    | Asleep of System.DateTime
    | Awake of System.DateTime
    | Nonsense

  let recordRegex = @"^\[([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-3]{2}):([0-9]{2})] (.*)$"

  let (|FirstRegexGroup|_|) pattern input =
   let m = Regex.Match(input,pattern) 
   if (m.Success) then Some m.Groups.[1].Value else None  

  let sortLog log = List.sortBy (fun el -> 
    match el with
    | LogEntry.Asleep dt -> dt
    | LogEntry.Awake dt -> dt
    | LogEntry.NewGuard (dt, _) -> dt
    | LogEntry.Nonsense -> new System.DateTime()) log

  let scrapeLog inputs =
     List.map (fun logEntry ->
      let m = Regex.Match(logEntry, recordRegex)
      if m.Success then
        let dt =
          new System.DateTime(
            (m.Groups.[1].Value |> System.Convert.ToInt32),
            (m.Groups.[2].Value |> System.Convert.ToInt32),
            (m.Groups.[3].Value |> System.Convert.ToInt32),
            (m.Groups.[4].Value |> System.Convert.ToInt32),
            (m.Groups.[5].Value |> System.Convert.ToInt32),
            0)

        match m.Groups.[6].Value with
        | "falls asleep" -> LogEntry.Asleep (dt)
        | "wakes up" -> LogEntry.Awake (dt)
        | FirstRegexGroup "Guard #([0-9]+) begins shift" badge -> LogEntry.NewGuard (dt, System.Convert.ToInt32 badge)
        | _ -> LogEntry.Nonsense
      else LogEntry.Nonsense) inputs
      |> sortLog
    
 
  let first (a, _, _) = a
  let second (_, b, _) = b
  let third (_, _, c) = c

  type MinuteCounts = Map<int, int>

  let getHighest minuteCounts =
    Map.fold (fun s k v -> if v > snd s then (k, v) else s) (0, 0) minuteCounts

  type SingleEntry =
    { TotalAsleep : int
      MinuteCounts : MinuteCounts }
  type SleepLog = Map<int, SingleEntry>
  
  let tickMinute (mcs: MinuteCounts) m =
    match (mcs.TryFind m) with
    | Some x -> Map.add m (x + 1) mcs
    | None -> Map.add m 1 mcs

  let addRangeToLog sleep wake sleepLogEntry =
    let timeAsleep = wake - sleep
    let eachMinute = [ for i in sleep..wake - 1 do yield i ]
    { TotalAsleep = sleepLogEntry.TotalAsleep + timeAsleep;
      MinuteCounts = List.fold tickMinute sleepLogEntry.MinuteCounts eachMinute }

  let processLog log =
    List.fold
      (fun s el ->
        match el with
        | LogEntry.NewGuard (_, badge) -> (Some badge, second s, third s)
        | LogEntry.Awake dt ->
          match (first s) with
          | Some badge ->
            match (second s) with
            | Some sleepTime ->
              let map: SleepLog = third s 
              let newMap =
                match map.TryFind badge with
                | Some x -> Map.add badge (addRangeToLog sleepTime dt.Minute x) map
                | None -> Map.add badge (addRangeToLog sleepTime dt.Minute {TotalAsleep = 0; MinuteCounts = (new Map<int, int> (Seq.empty))}) map
              (first s, None, newMap)
            | None -> s // THIS IS AN ERROR IF WE HIT THIS BRANCH - there was no "time asleep" stored for a "wakes up" entry
          | None -> s // THIS IS AN ERROR IF WE HIT THIS BRANCH - there was no active badge for a "wakes up" entry
        | LogEntry.Asleep dt -> (first s, Some dt.Minute, third s)
        | LogEntry.Nonsense -> s)
      (None, None, new Map<int, SingleEntry> (Seq.empty)) // Active Badge, Minute Fell Asleep, SleepLog
      log
    
  let getSleepLog fileName =
    getContents fileName
    |> scrapeLog
    |> processLog
    |> third

module part1 =
  open util

  let getLongestAsleep sleepLog =
    sleepLog
    |> Map.fold (fun s k v -> if v.TotalAsleep > first s then (v.TotalAsleep, k, getHighest v.MinuteCounts) else s) (0, 0, (0, 0))

  let execute fileName =
    let res = getSleepLog fileName |> getLongestAsleep
    (second res) * (fst (third res))

module part2 =
  open util
  let getMostFrequentAsleep sleepLog =
    sleepLog |> Map.fold (fun s k (v: SingleEntry) ->
      let h = getHighest v.MinuteCounts
      if snd h >= snd (snd s) then
        (k, h)
      else s
      ) (0, (0, 0))

  let execute fileName =
    let res = getSleepLog fileName |> getMostFrequentAsleep
    (fst res) * (fst (snd res))

module run =
  let sample = "inputs/sample4.txt"
  let real = "inputs/real4.txt"
  let displayResults inputFile =
    part1.execute inputFile |> printfn "Part 1 result: %i"
    part2.execute inputFile |> printfn "Part 2 result: %i"

  let runBoth =
    printfn "Sample:"
    displayResults sample
    printfn "Real:"
    displayResults real