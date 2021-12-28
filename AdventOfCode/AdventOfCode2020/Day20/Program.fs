
open System
open System.IO

type Tile = { Id : int; Lines : string[] }
type Edge = { Text : string; Flipped : bool;  Tile :int }
    with static member Create id (str:string) = 
            let reverse = String(str.ToCharArray() |> Array.rev)
            if (reverse < str) then {Text = reverse; Tile = id; Flipped = true}
            else {Text = str; Tile = id; Flipped = false}

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

let edges = ResizeArray(tiles.Count * 4)
for tile in tiles do
    edges.Add(Edge.Create tile.Id tile.Lines.[0])
    edges.Add(Edge.Create tile.Id tile.Lines.[tile.Lines.Length - 1])
    // Left
    edges.Add(Edge.Create tile.Id (tile.Lines |> Array.map (fun l -> l.[0]) |> String))
    // Right
    edges.Add(Edge.Create tile.Id (tile.Lines |> Array.map (fun l -> l.[l.Length - 1]) |> String))

let matchingEdges = 
    edges.ToArray() 
    |> Array.groupBy (fun x -> x.Text) 
    // Remove duplicate
    |> Array.map (fun (count, edges) -> (count, edges |> Array.distinctBy (fun x -> x.Tile)))
    |> Map.ofSeq

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

// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"
