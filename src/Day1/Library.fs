namespace Day1
open System

module util =
  let getFrequencies fileName =
    let lines = IO.File.ReadLines(fileName)
    lines
      |> Seq.map Convert.ToInt32
      |> Seq.toList

module part1 =
  let rec addFreq acc s =
    match s with
      | [] -> acc
      | freq::freqs -> addFreq (acc + freq) freqs

  let execute fileName =
    util.getFrequencies fileName |> addFreq 0


module part2 =

  let rec addFreqWithState acc visited whole remaining =
    match remaining with
      | [] -> addFreqWithState acc visited whole whole
      | head::tail ->
        let newval = acc + head
        if Set.contains newval visited then
          newval
        else
          addFreqWithState newval (Set.add newval visited) whole tail
  let execute fileName =
    let freqs = util.getFrequencies fileName
    addFreqWithState 0 (new Set<int> (Seq.empty)) freqs freqs