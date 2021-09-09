// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

let maxIterations = 30000000

[<Struct>]
type State = { LastUsed : int[] ; Next :int }

let init numbers = {
    LastUsed = 
        numbers
        |> Seq.take (Array.length numbers - 1)
        |> Seq.indexed
        |> Seq.fold (fun state (idx, num) ->
            state.[num] <- (idx+1) 
            state
        ) (Array.zeroCreate<int> maxIterations);
    Next = Array.last numbers
    }

let next prev turn (state:int[]) =
    match state.[prev] with
    | 0 -> 0
    | lastUsed -> (turn - lastUsed)

let step turn state =
    let n = next state.Next turn state.LastUsed 
    state.LastUsed.[state.Next] <- turn 
    {LastUsed = state.LastUsed; Next = n}    
    

[<EntryPoint>]
let main argv =

    IO.File.ReadAllLines "sample.txt"
    |> Seq.iter (fun row -> 
        let input = 
            row.Split(',')
            |> Array.map Int32.Parse

        let mutable state = init input
        for i in input.Length..2019 do
            state <- step i state
        
        let num2020 = state.Next
        for i in 2020..(30000000 - 1) do
            state <- step i state

        printfn "Input %A gives %d and %d" input num2020 state.Next
    )
    0 // return an integer exit code