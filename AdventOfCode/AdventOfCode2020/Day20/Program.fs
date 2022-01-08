
open System
open System.IO
open System.Linq
open System.Collections.Generic

let strrev (str:string) = String.mapi (fun i _ -> str.[str.Length - i - 1]) str  

type Dir = L | R | U | D

type Edge = { Text : string; Flipped : bool;  Tile :int; Dir : Dir }
with 
    static member Create id (str:string) (dir:Dir) = 
        let reverse = strrev str
        if reverse.AsSpan().SequenceCompareTo(str) < 0 
            then {Text = reverse; Tile = id; Flipped = true; Dir = dir}
            else {Text = str; Tile = id; Flipped = false; Dir = dir}

type Tile = { Id : int; Lines : string[] }
    with
        member tile.Top = Edge.Create tile.Id tile.Lines.[0] U
        member tile.Bottom = Edge.Create tile.Id tile.Lines.[tile.Lines.Length - 1] D
        member tile.Left = Edge.Create tile.Id (tile.Lines |> Array.map (fun l -> l.[0]) |> String) L
        member tile.Rigth = Edge.Create tile.Id (tile.Lines |> Array.map (fun l -> l.[l.Length - 1]) |> String) R

        static member flipV tile = {tile with Lines = tile.Lines |> Array.rev}
        static member flipH tile = {tile with Lines = tile.Lines |> Array.map strrev}
        
        static member rotate tile = 
            { tile with Lines = 
                        tile.Lines 
                        |> Array.mapi (fun y line -> 
                            line |> String.mapi (fun x _ -> tile.Lines[x][y])) }
        
let parseInput path =
    //use file = File.OpenText file
    let lines = File.ReadAllLines path
    let mutable i = 0

    let l = ResizeArray()
    while (uint i) < (uint lines.Length) (*lines.[i].StartsWith("Tile") *)do
        let id = lines.[i].Split(' ', ':').[1] |> int
        let nextEmpty = Array.FindIndex(lines, i, (fun l -> String.IsNullOrEmpty(l)))
        let endIndex = if (nextEmpty > 0) then nextEmpty - 1 else lines.Length - 1 
        let lines =  lines.[i + 1 .. endIndex ]
        l.Add({Id = id; Lines = lines})
        i <- endIndex + 2
    l


let tiles = parseInput "input.txt"
let tileMap = tiles.ToDictionary(fun t -> t.Id)

let edges = ResizeArray(tiles.Count * 4)
for tile in tiles do
    edges.Add(tile.Top)
    edges.Add(tile.Bottom)
    edges.Add(tile.Left)
    edges.Add(tile.Rigth)

let matchingEdges = 
    edges.ToArray() 
    |> Array.groupBy (fun x -> x.Text) 
    // Remove duplicate
    |> Array.map (fun (count, edges) -> (count, edges |> Array.distinctBy (fun x -> x.Tile)))
    |> Map.ofSeq


let neighbour edge = 
    let e = matchingEdges[edge.Text].SingleOrDefault(fun e -> e.Tile <> edge.Tile)
    if obj.ReferenceEquals(null, e) 
    then None
    else Some tileMap[e.Tile]


let corners = 
    let edges = matchingEdges.Values
                // Get edges
                |> Seq.choose (fun matches -> if (matches.Length = 1) then Some matches.[0].Tile else None)
                |> Seq.countBy id
                |> Seq.toArray
    edges |> Array.filter (fun (tile, count) -> count = 2) |> Array.map (fun (tile, count) -> uint64 tile)

printfn "Input %A" (tiles.ToArray())

printfn "Corners %A" corners
printfn "Product is %d" (corners |> Array.fold (*) 1UL)


// STEP 2 - Setup layout

let rec orientToHorizontal (target:Edge) (tile:Tile) =
    if target.Text = tile.Left.Text then 
        if (target.Flipped = tile.Left.Flipped) then tile else Tile.flipV tile
    else if target.Text = tile.Rigth.Text then
        orientToHorizontal target (Tile.flipH tile)
    else // up or down
        orientToHorizontal target (Tile.rotate tile)

let rec orientToVertical (target:Edge) (tile:Tile) =
    if target.Text = tile.Top.Text then 
        if (target.Flipped = tile.Top.Flipped) then tile else Tile.flipH tile
    else if target.Text = tile.Bottom.Text then
        orientToVertical target (Tile.flipV tile)
    else // right or left
        orientToVertical target (Tile.rotate tile)

let isEdge (edge:Edge) = matchingEdges[edge.Text].Length = 1

let orientFirstEdge (tile:Tile) =
    match (isEdge tile.Top, isEdge tile.Left, isEdge tile.Rigth) with
    | (true, true, _) -> tile
    | (true, false ,true) -> Tile.flipH tile
    | (false, true, false) -> Tile.flipV tile //Left and bottom 
    | (false, false, true) ->  Tile.flipH (Tile.flipV tile) // Right and bottom
    | _ -> failwith "Fail"

let layout = Dictionary<(int*int),Tile>()
let start = orientFirstEdge tileMap[corners[0] |> int]
layout[(0,0)] <- start

let rec fillRow x y (left:Edge) =
    match neighbour left with 
    | None -> ()
    | Some tile -> 
        let rightTile = orientToHorizontal left tile
        layout.Add((x,y), rightTile)
        fillRow (x + 1) y rightTile.Rigth

let rec fillRows y (top:Edge) =
    match neighbour top with 
    | None -> ()
    | Some tile -> 
        let rightTile = orientToVertical top tile
        layout.Add((0,y), rightTile)
        fillRow 1 y rightTile.Rigth
        fillRows (y+1) rightTile.Bottom


fillRow 1 0 start.Rigth
fillRows 1 start.Bottom


let maxCoord = layout.Keys.Max()
printfn "Max coord = %A" maxCoord
printfn "Layout = %A" layout

// Step 2 - Phase 2 Setup Image (with cropped widths)
let (xMax, yMax) = maxCoord
let perTile = tiles[0].Lines.Length - 2 // 8
assert (perTile = 8)
let widht = (xMax + 1) * perTile
let height = (yMax + 1) * perTile
let image = Array2D.init height widht (fun x y -> 
            let tileX = x / perTile
            let tileY = y / perTile
            let tile = layout[(tileX, tileY)]
            tile.Lines[(y % 8) + 1 ][(x % 8) + 1])


//*
//
//                  # 
//#    ##    ##    ###
// #  #  #  #  #  #
let seaMonster = """
                  # 
#    ##    ##    ###
 #  #  #  #  #  #   """
 
let seaMonsterLines = seaMonster.Split("\r\n", StringSplitOptions.RemoveEmptyEntries)
let firstOffset = seaMonsterLines[0].IndexOf '#'
let seaMonsterWidth = seaMonsterLines[0].Length

let matchSeaMonster (image:char[,]) x y =
    if (Array2D.get image (x + firstOffset) y) <> '#' 
    then false
    else 
        let rec matchLine (str:string) idx x y =
            if idx = seaMonsterWidth then true
            else if str[idx] = ' ' then matchLine str (idx+1) x y
            else if image[x + idx, y] = '#' then matchLine str (idx+1) x y
            else false
        (matchLine seaMonsterLines.[1] 0 x (y+1)
        && matchLine seaMonsterLines.[2] 0 x (y+2))

let countMonsters (image:char[,]) =

    //printfn "\n--- img ---:"
    //for y in 0 .. ((Array2D.length2 image) - 1) do
    //    for x in 0 .. ((Array2D.length1 image) - 1) do
    //        printf "%c" (image[y,x])
    //    printfn ""
    //printfn ""

    let mutable numMonstes = 0
    for x in 0.. ((Array2D.length1 image) - seaMonsterWidth) do
        for y in 0.. ((Array2D.length2 image) - 3) do
            if (matchSeaMonster image x y) then
                numMonstes <- numMonstes + 1
    printfn "monster count: %d" numMonstes
    numMonstes


let flipImage arr = 
    let len1 = (Array2D.length1 arr)
    let len2 = (Array2D.length2 arr)
    Array2D.init len1 len2  (fun x y -> arr[(len1 - x) - 1, y])

    
let rotateImage arr = 
    let len1 = (Array2D.length1 arr)
    let len2 = (Array2D.length2 arr)
    Array2D.init len1 len2  (fun x y -> arr[y, x])

let mutable img = image

let mutable count = 0 

for i in 1..4 do
    count <- Math.Max(count, (countMonsters img))
    img <- flipImage img
    count <- Math.Max(count, (countMonsters img))
    img <- rotateImage img


let mutable numHash = 0
image |> Array2D.iter (fun x -> if x = '#' then numHash <- numHash + 1)

let monsterHash = seaMonster.ToCharArray() |> Array.sumBy (fun x -> if x = '#' then 1 else 0)
let roughtNess = numHash - count * monsterHash

printfn "NumHash %d MonsterHash %d MonsterCoubnt %d " numHash monsterHash count
printfn "Roughness %d" roughtNess
 




// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"
