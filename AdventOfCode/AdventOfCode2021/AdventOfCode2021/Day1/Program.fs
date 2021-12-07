// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO


[<EntryPoint>]
let main argv =

    let input = 
        File.ReadAllLines "In.txt" 
        |> Array.map Int32.Parse

    //let numIncreases =
    //    Array.zip input (Array.skip 1 input)
    //    |> Array.sumBy (fun (a,b) -> if a < b then 1 else 0)
    let countIncreases (arr:int[]) = 
        let mutable c  = 0
        for i in 1..arr.Length-1 do
            c <- if (arr.[i] > arr.[i - 1]) then (c + 1) else c
        c

    printfn "Increases %d" (countIncreases input)

    let window3sum =
        input |> Array.windowed 3 |> Array.map Array.sum

    printfn "Increases %d" (countIncreases window3sum)

    0 // return an integer exit code