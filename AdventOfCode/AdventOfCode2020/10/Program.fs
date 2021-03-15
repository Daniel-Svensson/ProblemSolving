// Learn more about F# at http://fsharp.org

open System
open System.IO

[<EntryPoint>]
let main argv =

    let input = 
        File.ReadAllLines "input.txt"
        |> Array.map Int32.Parse
        
    Array.sortInPlace input
    let adapters = Array.concat [
            [|0|]; 
            input; 
            [|3 + (Array.last input)|]
        ]

    let rec getDifferences idx prev (counts:int array) = 
        if (idx = adapters.Length) then counts
        else 
            let item = adapters.[idx]
            let diff = item - prev
            counts.[diff] <- counts.[diff] + 1 
            getDifferences (idx + 1) item counts

    let counts = 
        Array.zeroCreate<int> 4
        |> getDifferences 1 0

    
    printfn "counts %A multiplied %d" counts (counts.[1] * counts.[3]) 


    // DP problem
    // d(x) där x är en adapter
    // -3 -2 -1 0
    // d( <= 0 ) = 1
    let d = Array.zeroCreate<UInt64> adapters.Length
    d.[0] <- 1UL

    let calcCombinations idx =
        let current = adapters.[idx]
        let rec getCombinations i sum = 
            if i < 0 || adapters.[i] < current - 3 then sum
            else getCombinations (i-1)  (sum + d.[i]) 

        getCombinations (idx - 1) 0UL

    for i in 1..adapters.Length - 1 do
        d.[i] <- calcCombinations i

    printfn "d = %A\n%d" d (Array.last d)

    0 // return an integer exit code
