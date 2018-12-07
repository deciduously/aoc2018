namespace Day6

module util =
  let scrapeCoords inputFile =
    System.IO.File.ReadLines(inputFile)
    |> Seq.mapi (fun i el ->
      let coords = el.Split ','
      (i, (coords.[0] |> System.Convert.ToInt32, coords.[1] |> System.Convert.ToInt32)))
    |> List.ofSeq

  let labels = [ for c in ['a' .. 'z'] @ ['A' .. 'Z'] do yield c ]

  type Coord = int * (int * int)
  let getX = snd >> fst
  let getY = snd >> snd
  let eqCoord origin target = fst origin = fst target && snd origin = snd target

  let manhattanDistance (origin: int * int) (target: int * int) =
    System.Math.Abs (fst origin - fst target) + System.Math.Abs (snd origin - snd target)

  let gridSize (points: Coord list) =
    let across = List.map getX points |> List.sort |> List.rev |> List.head
    let down = List.map getY points |> List.sort |> List.rev |> List.head
    (across, down)
  
  let distanceGrid points =
    let (across, down) = gridSize points
    Array2D.mapi (fun down across c ->
      let nearest =
        points
        |> List.fold (fun s el -> Map.add (fst el) (manhattanDistance (across, down) (snd el)) s) (new Map<int, int> (Seq.empty))
        |> Map.fold (fun s k v -> if v < snd s then (k, v) else s) (across, System.Math.Max(across, down))
      if List.exists (fun el -> eqCoord (snd el) (across, down)) points then labels.[fst nearest + 26] else labels.[fst nearest]) (Array2D.create down across '.')
  
  let tickMap c (map: Map<char, int>) =
    match (map.TryFind c) with
    | Some x -> Map.add c (x + 1) map
    | None -> Map.add c 1 map

  let largetArea distanceGrid =
    distanceGrid
    |> Seq.cast<char>
    |> Seq.fold (fun s el -> tickMap el s) (new Map<char, int> (Seq.empty))
    |> Map.toList
    |> List.sortBy snd
    |> List.head
    |> snd

  let renderGrid dg = 
    Array2D.iteri (fun x y el ->
      printf "%c" el
      if y = Array2D.length2 dg - 1 then
        printf "\n"
      else
        printf "") dg

module part1 =
  open util
  let execute inputFile =
    let dg = scrapeCoords inputFile |> distanceGrid
    renderGrid dg
    largetArea dg
  
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