// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

type Pos = (struct (int * int))
type FoldDirection = Up | Left

type Fold = FoldDirection * int

let parsePos (str:string) =
    match str.Split(',') with
    | [|x; y|] -> Pos (int x,int  y)
    | _ -> failwith "invalid pos"

let parseFold (str:string) =
    match str.Split(' ', '=') with
    | [|"fold"; "along"; "x"; x|] -> Fold (Left, int x)
    | [|"fold"; "along"; "y"; y|] -> Fold (Up, int y)
    | _ -> failwith "invalid fold"
    

let parseFile filename = 
    let lines = File.ReadAllLines filename
    let coordinates = 
        lines 
        |> Array.takeWhile (fun x -> not (String.IsNullOrEmpty x))
        |> Array.map parsePos
    let folds = 
        lines
        |> Array.skip (coordinates.Length + 1)
        |> Array.map parseFold
    (Set.ofArray coordinates, folds)

let fold (coordinates:Set<Pos>) (fold:Fold) =
    let folder = 
        match fold with
        | (Up, fy) -> (fun struct (x,y) -> if (y > fy) then struct(x, fy - (y - fy)) else struct (x,y)) 
        | (Left, fx) -> (fun struct (x,y) -> if (x > fx) then struct(fx - (x - fx), y) else struct (x,y))
    coordinates |> Set.map folder

let print (s:Set<Pos>) =
    let a = Set.toArray s
    let xM : int = a |> Array.map (fun struct (x,_) -> x) |> Array.max
    let yM : int = a |> Array.map (fun struct (_,y) -> y) |> Array.max

    for y in 0..yM do
        for x in [0..xM] do
            let ch = if (Set.contains (Pos (x,y)) s) then '#' else '.'
            printf "%c"ch
        printfn ""

[<EntryPoint>]
let main argv =
    
    let (dots, folds) = parseFile "In.txt"

    let res1 = fold dots folds.[0]
    
    //print res1

    let mutable state = res1

    for i in 1 .. (folds.Length - 1) do
        state <- fold state folds.[i]
        printfn "Dots after %d is %d" i (Set.count res1)

    print state

    //printfn "\nStep2:"
    //print (fold res1 folds.[1])



    0 // return an integer exit code