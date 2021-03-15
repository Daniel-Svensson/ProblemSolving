// Learn more about F# at http://fsharp.org

open System


type Boat = { PosX: int; PosY:int; DirX: int; DirY:int; }

//type State = { PosX: int; PosY:int; BoatX:int; BoatY:int;}

type Action = 
    | North
    | South
    | East
    | West
    | Left
    | Right
    | Forward

[<Struct>]
type Command = { Action : Action; Value : int }

let parseLine (str:string) =
    let amount = str.[1..] |> Int32.Parse
    let action = match str.[0] with
        | 'N' -> North
        | 'S' -> South
        | 'E' -> East
        | 'W' -> West
        | 'L' -> Left
        | 'R' -> Right
        | 'F' -> Forward
        | _ -> failwith "invalid action"
    {Action = action; Value = amount }


let moveDir state dx dy = 
    { state with DirX = (state.DirX + dx); DirY = (state.DirY+ dy)}

let moveBoat state dx dy = 
    { state with PosX = (state.PosX + dx); PosY = (state.PosY + dy)}

let step move state command =
    let turnR state angle =
        match angle with
            | 90 -> { state  with DirX = state.DirY; DirY = -state.DirX}
            | 180 -> { state  with DirX = -state.DirX; DirY = -state.DirY}
            | 270 -> { state  with DirX = -state.DirY; DirY = state.DirX}
            | x -> failwithf "turnR %d is not implemented" x

    match command.Action with
    | North -> move state 0 command.Value
    | South -> move state 0 -command.Value
    | West -> move state -command.Value 0
    | East -> move state command.Value 0
    | Forward -> moveBoat state (command.Value* state.DirX) (command.Value*state.DirY)
    | Right -> turnR state command.Value
    | Left ->  turnR state (360-command.Value)



[<EntryPoint>]
let main argv =

    let startPosition = {PosX = 0; PosY=0; DirX=1; DirY=0; }
    
    let eval move input =
        input
        |> Array.map parseLine
        |> Array.fold move startPosition

    let input = IO.File.ReadAllLines "sample.txt"
    
    input
    |> Array.map parseLine
    |> Array.fold (step moveBoat) startPosition
    |> (fun finalPos ->
        printfn "Final state is %A with distane %d" finalPos (abs(finalPos.PosX) + abs(finalPos.PosY))
    )

    let startPosition = {PosX = 0; PosY=0; DirX=10; DirY=1; }
    input
    |> Array.map parseLine
    |> Array.fold (step moveDir) startPosition
    |> (fun finalPos ->
    printfn "Final state is %A with distane %d" finalPos (abs(finalPos.PosX) + abs(finalPos.PosY))
    )
    
    

    0 // return an integer exit code
