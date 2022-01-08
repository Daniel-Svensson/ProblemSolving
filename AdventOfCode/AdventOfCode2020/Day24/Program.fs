open System
open System.IO
open System.Collections.Generic

type Direction = W | E | NW | NE | SW | SE
[<Struct>] type Coordinate = { x: int; y: int }
type Color = Black | White

let toDirection (str:string) = 
    seq {
        let mutable i = 0
        while i < str.Length do
            yield match str[i] with
                | 'w' -> W
                | 'e' -> E
                | 'n' ->
                    i <- i + 1
                    match str[i] with
                    | 'w' -> NW
                    | 'e' -> NE
                    | _ -> failwith "no w or e"
                | 's' ->
                    i <- i + 1
                    match str[i] with
                    | 'w' -> SW
                    | 'e' -> SE
                    | _ -> failwith "no w or e"
                | _ -> failwith "invalid"
            i <- i + 1
    }

let move coord dir =
    match dir with
    | W -> {coord with x = coord.x - 1 } 
    | E -> {coord with x = coord.x + 1 }    
    | NW -> {coord with y = coord.y + 1 }    
    | NE -> {coord with y = coord.y + 1; x = coord.x + 1 }
    | SW -> {coord with y = coord.y - 1; x = coord.x - 1 }    
    | SE -> {coord with y = coord.y - 1}

let lines = File.ReadAllLines "input.txt"

// Part 1
let positions1 =
    lines
    |> Array.map (fun line -> (toDirection line) |> Seq.fold move {x=0;y=0} )

let flipColor c =
    match c with
    | Some White -> Some Black
    | Some Black -> Some White
    | None -> Some Black


let state = Array.fold (fun map pos -> Map.change pos flipColor map) Map.empty positions1
//System.Runtime.InteropServices.CollectionsMarshal.GetValueRefOrNullRef
let allBlack = state |> Map.filter (fun key color ->  color = Black)
let numBlack = allBlack.Count
printfn "Number of black is %d" numBlack


let allDir = [|W;E; NE; NW;SE;SW |]

let neighbours coord = 
    allDir |> Array.map (fun dir -> (move coord dir))
    
let numBlackNeighbours (blacks:HashSet<Coordinate>) coord = 
    neighbours coord
    |> Array.sumBy (fun c -> if blacks.Contains c then 1 else 0)

let step (blacks:HashSet<Coordinate>) = 
    let relevantTiles = HashSet blacks
    for tile in blacks do
        allDir |> Array.iter (fun dir -> relevantTiles.Add (move tile dir) |> ignore)
    
    let newBlacks = HashSet relevantTiles.Count
    for tile in relevantTiles do
        let numBlack = numBlackNeighbours blacks tile
        let isBlack = blacks.Contains tile 
        if (isBlack && (numBlack = 1 || numBlack = 2)) || (not isBlack && numBlack = 2) then
            newBlacks.Add tile |> ignore
    newBlacks
    
let mutable blacks = HashSet allBlack.Keys 
for day in 1..100 do
    blacks <- step blacks
    printfn "Day %d: %d" day blacks.Count
    
// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"
