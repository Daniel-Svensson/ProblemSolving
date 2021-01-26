// Learn more about F# at http://fsharp.org

open System
open System.IO

[<EntryPoint>]
let main argv =
    
    let numbers = 
        File.ReadAllLines("input.txt")
        |>  Array.map Int32.Parse

    let (i1,i2) = 
        Array.allPairs numbers numbers
        |> Array.find (fun (i1, i2) -> i1 + i2 = 2020)

    printf $"The sum is {i1*i2} of ({i1} and {i2})"

    0 // return an integer exit code
