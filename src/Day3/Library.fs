namespace Day3
open System.Text.RegularExpressions

module util =
  // cell contents are a list of ClaimIds
  let grid size = Array2D.create size size []

  let claimRegex = @"#(?<ClaimNum>[0-9]+) @ (?<xCoord>[0-9]+),(?<yCoord>[0-9]+): (?<rows>[0-9]+)x(?<columns>[0-9]+)"

  let fallsInClaim claimX claimY rows columns gridX gridY =
    (gridX >= claimX && gridX < rows + claimX) && (gridY >= claimY && gridY < columns + claimY)

  // Given a claim string and a grid, return a new grid with the claims added
  let claim s g =
    let matches = Regex.Match(s, claimRegex)
    if matches.Success then
      let claimNum = matches.Groups.["ClaimNum"].Value |> System.Convert.ToInt32
      let claimX = matches.Groups.["xCoord"].Value |> System.Convert.ToInt32
      let claimY = matches.Groups.["yCoord"].Value |> System.Convert.ToInt32
      let rows = matches.Groups.["rows"].Value |> System.Convert.ToInt32
      let columns = matches.Groups.["columns"].Value |> System.Convert.ToInt32
      Array2D.mapi (fun i j cell -> if fallsInClaim claimX claimY rows columns i j then cell @ [claimNum] else cell) g
    else
      g
  let readClaims fileName =
    System.IO.File.ReadLines(fileName) |> List.ofSeq

  let applyClaims fileName =
    let g = grid 1000
    let claims = readClaims fileName
    List.fold (fun accGrid c -> claim c accGrid) g claims
    |> Seq.cast<int list>

module part1 =
  let execute fileName =
    util.applyClaims fileName
    |> Seq.filter (fun el -> List.length el > 1)
    |> Seq.length

module part2 =
  // check if a given claim has any overlaps
  let noOverlaps claim g =
    g
    |> Seq.filter (fun cell -> List.contains claim cell)
    |> Seq.forall (fun cell -> List.length cell = 1)
    
  let execute fileName =
    let claims = util.readClaims fileName
                |> Seq.map (fun el ->
                  let matches = Regex.Match(el, util.claimRegex)
                  matches.Groups.["ClaimNum"].Value |> System.Convert.ToInt32)
                |> List.ofSeq
    let g = util.applyClaims fileName
    let res = List.fold (fun s el -> if noOverlaps el g then s @ [el] else s) [] claims
    res.[0]