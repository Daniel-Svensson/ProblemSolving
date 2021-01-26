// Learn more about F# at http://fsharp.org

open System
open System.IO 


let countTrees (rows:string array) (dx,dy) =
    let rec countTreesImpl x y sum =
        if y >= rows.Length then sum
        else 
            let row = rows.[y]
            let isTree = row.[x % row.Length] = '#'
            countTreesImpl (x + dx) (y + dy) (if isTree then sum + 1UL else sum)
    countTreesImpl 0 0 0UL

[<EntryPoint>]
let main argv =
    
    let input = File.ReadAllLines("input.txt")

    let trees = countTrees input (3,1)
    printfn  $"found {trees}"

    let slopes = [(1,1); (3,1); (5,1); (7,1); (1,2)]
    slopes 
    |> Seq.map (countTrees input)
    |> Seq.fold (fun acc count -> acc * count) 1UL
    |> printfn "muliply %d"

    0 // return an integer exit code
