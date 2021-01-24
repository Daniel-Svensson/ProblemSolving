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
    
    let allPairs3 (arr: int array) =
        seq {
            for i in 0..(arr.Length - 1)  do
            for j in i+1..(arr.Length - 1) do
            for k in j+1..(arr.Length - 1) do
                yield  (arr.[i], arr.[j], arr.[k])
        }

    let (i1, i2, i3) = 
        allPairs3 numbers
        |> Seq.find (fun (i1, i2, i3) -> i1 + i2 + i3 = 2020)

    printf $"The sum is {i1 * i2 * i3} of ({i1} and {i2} and {i3})"

    0 // return an integer exit code
