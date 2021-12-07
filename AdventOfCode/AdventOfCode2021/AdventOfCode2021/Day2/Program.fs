// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

type Direction = Forward | Up | Down
type Action = {Direction: Direction; Units : uint}
type State = {Distance: uint; Depth : uint }

let parseAction (str:string) =
    let parts = str.Split(' ')
    let direction = 
        match parts.[0] with
        | "up" -> Up
        | "down" -> Down
        | "forward" -> Forward
        | _ -> failwith "invalid direction"
    {Direction = direction; Units = (uint parts.[1])}

let move state action =
    match action.Direction with
    | Up when state.Depth >= action.Units -> {state with Depth = state.Depth + action.Units} 
    | Down -> {state with Depth = state.Depth + action.Units} 
    | Forward -> {state with Distance = state.Distance + action.Units} 
    | _ -> failwith "Invalid move"

// part 2
type State2 = {Distance: uint; Depth : uint; Aim : uint}
let move2 state action =
    match action.Direction with
    | Up -> {state with Aim=  state.Aim - action.Units} 
    | Down -> {state with Aim = state.Aim + action.Units} 
    | Forward -> {state with Distance = state.Distance + action.Units; Depth = state.Depth + state.Aim * action.Units} 

[<EntryPoint>]
let main argv =

    let solve1 file = 
        File.ReadAllLines file 
        |> Array.map parseAction
        |> Array.fold move {Distance = 0u; Depth = 0u}
        |> (fun state -> printfn "%s position is %A, result %d" file state (state.Distance * state.Depth))

    solve1 "Sample.txt"    
    solve1 "In.txt"

    let solve2 file = 
        File.ReadAllLines file 
        |> Array.map parseAction
        |> Array.fold move2 {Distance = 0u; Depth = 0u; Aim = 0u}
        |> (fun state -> printfn "%s position is %A, result %d" file state (state.Distance * state.Depth))

        
    solve2 "Sample.txt"    
    solve2 "In.txt"

    0 // return an integer exit code