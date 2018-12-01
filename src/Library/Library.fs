module Library
open System

let rec addFreq acc s =
  match s with
    | [] -> acc
    | freq::freqs -> addFreq (acc + freq) freqs

let rec addFreqWithState acc visited whole remaining =
  match remaining with
    | [] -> addFreqWithState acc visited whole whole
    | head::tail ->
      let newval = acc + head
      if Set.contains newval visited then
        newval
      else
        addFreqWithState newval (Set.add newval visited) whole tail

let getFrequencies fileName =
  let lines = IO.File.ReadLines(fileName)
  lines
    |> Seq.map Convert.ToInt32
    |> Seq.toList

let day1Part1 fileName =
  getFrequencies fileName |> addFreq 0

let day1Part2 fileName =
  let freqs = getFrequencies fileName
  addFreqWithState 0 (new Set<int> (Seq.empty)) freqs freqs