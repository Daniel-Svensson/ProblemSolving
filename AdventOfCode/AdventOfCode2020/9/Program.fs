// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =

//let input =
//    IO.File.ReadAllLines "sample.txt"
//    |> Array.map UInt64.Parse

  //  let preambleLenght = 5

    let input =
        IO.File.ReadAllLines "input.txt"
        |> Array.map UInt64.Parse

    let preambleLenght = 25

    let pickIfInvalid length idx =
          let item = input.[idx]
          let preamble = input.[(idx - length) .. (idx - 1)]
          
          Seq.allPairs preamble preamble
          |> Seq.exists (fun (x,y) -> x + y = item)
          |> function
            | true -> None
            | false -> Some item

    let firstInvalid =
        [preambleLenght .. input.Length - 1]
        |> Seq.pick (pickIfInvalid preambleLenght)

 
    
    printfn "First invalid is %A" firstInvalid

    let tryFindRange (start:int) =
        let rec tryFindRangeInner idx acc = 
            let acc = acc + input.[idx]
            if (acc > firstInvalid) then None
            else if (acc = firstInvalid) then Some (start, idx)
            else // if (acc < firstInvalid)
                tryFindRangeInner (idx + 1) acc
        tryFindRangeInner (start + 1) input.[start]

    [0 .. (input.Length - 2)]
    |> Seq.choose tryFindRange
    |> Seq.iter (fun (i1,i2) -> 
        let range = input.[i1..i2]
        let min = Array.min range
        let max = Array.max range
        let sum = min + max
        printfn "%d + %d = %d ,  Res %A" min max sum (i1,i2) 
        )

    printfn "Hello World from F#!"
    0 // return an integer exit code
