module Library
open System

let rec addFreq acc s =
  match s with
    | [] -> acc
    | freq::freqs -> addFreq (acc + freq) freqs

let day1Part1 fileName =
  let lines = System.IO.File.ReadLines(fileName)
  lines
    |> Seq.map Convert.ToInt32
    |> Seq.toList
    |> addFreq 0