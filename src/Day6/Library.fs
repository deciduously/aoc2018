namespace Day6

module util =
  let scrapeCoords inputFile =
    System.IO.File.ReadLines(inputFile)
    |> Seq.mapi (fun i el ->
      let coords = el.Split ','
      (i, (coords.[0] |> System.Convert.ToInt32, coords.[1] |> System.Convert.ToInt32)))
    |> List.ofSeq

  type Coord = int * (int * int)
  let getX = snd >> fst
  let getY = snd >> snd

  let manhattanDistance (origin: int * int) (target: int * int) =
    System.Math.Abs (fst origin - fst target) + System.Math.Abs (snd origin - snd target)

  let maxBy f l = List.map f l |> List.sort |> List.rev |> List.head
  
  let distanceGrid points =
    let (across, down) = (maxBy getX points + 1, maxBy getY points + 1)
    Array2D.mapi (fun down across c ->
      let nearest =
        points
        |> List.fold (fun s el -> Map.add (fst el) (manhattanDistance (snd el) (across, down)) s) (new Map<int, int> (Seq.empty))
        |> Map.fold (fun (s: (int * int) list) k v -> if v < snd s.[0] then [(k, v)] else if v = snd s.[0] then [(k, v)] @ s else s) [(-1, across * down)]
      if List.exists (fun el -> (snd el) = (across, down)) points then
        fst nearest.[0]
      else if List.length nearest > 1 then c else fst nearest.[0]) (Array2D.create down across -1)
  
  let tickMap c (map: Map<int, int>) =
      match (map.TryFind c) with
      | Some x -> Map.add c (x + 1) map
      | None -> Map.add c 1 map

  // Can you do findFinite and getAreas in one pass?

  let findFinite dg areaMap =
    let mutable candidates = areaMap;
    Array2D.iteri (fun down across id ->
      if down = 0 || down = Array2D.length1 dg - 1 || across = 0 || across = Array2D.length2 dg - 1 then
        candidates <- Map.remove id candidates
      else
        ()) dg
    candidates

  let getAreas dg =
    let mutable arrMap = new Map<int, int> (Seq.empty)
    Array2D.iter (fun i -> arrMap <- tickMap i arrMap) dg
    let res = findFinite dg arrMap |> Map.toList
    printfn "%A" res
    res |> maxBy snd

  // only used to debug on the sample
  // breaks on the whole anyway - there more than 26 points!
  let renderGrid dg =
    let labels = [ for c in ['a' .. 'z'] @ ['A' .. 'Z'] do yield c ]
    Array2D.iteri (fun _ y el ->
      if el < 0 then printf "." else printf "%c" labels.[el]
      if y = Array2D.length2 dg - 1 then
        printf "\n"
      else
        printf "") dg

module part1 =
  open util
  let execute inputFile =
    let dg = scrapeCoords inputFile |> distanceGrid
    //renderGrid dg
    getAreas dg
  
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