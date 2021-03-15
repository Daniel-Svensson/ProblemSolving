// Learn more about F# at http://fsharp.org

open System

[<Literal>]
let EMPTY = 'L'
[<Literal>]
let OCCUPIED = '#'

[<EntryPoint>]
let main argv =

    let input = IO.File.ReadAllLines "input.txt"

    let seats = input |> Array.map (fun x -> x.ToCharArray())

    let isOccupied (state: char[][]) x y =
        y >= 0 && y < state.Length
            && x >= 0 && x < state.[y].Length 
            && state.[y].[x] = '#'

    let neighbors = 
        [|
            for x in -1..1 do
                for y in -1..1 do
                    if (x <> 0 || y <> 0) then yield (x,y) 
        |]

    let countNeighbors state x y =
        let mutable count = 0
        for (dx,dy) in neighbors do
            if isOccupied state (x + dx) (y + dy) then
                count <- count + 1
        count

    let rec checkDirection  (state: char[][]) x y dx dy = 
        y >= 0 && y < state.Length
        && x >= 0 && x < state.[y].Length 
        && match state.[y].[x] with
        | OCCUPIED -> true
        | EMPTY -> false
        | _ -> checkDirection state (x + dx) (y + dy) dx dy
            
    let countNeighbors2 state x y =
        let mutable count = 0
        for (dx,dy) in neighbors do
            if checkDirection state (x + dx) (y + dy) dx dy then
                count <- count + 1
        count

    let flipAll state =
        let newState = state |> Array.map Array.copy
        let mutable change = false
        for y in 0..state.Length - 1 do
            for x in 0..state.[y].Length - 1 do
                match state.[y].[x] with 
                | 'L' when (countNeighbors2 state x y) = 0 ->
                    newState.[y].[x] <- '#'
                    change <- true
                | '#' when (countNeighbors2 state x y) >= 5 ->
                    newState.[y].[x] <- 'L'
                    change <- true
                | _ -> ()
        (newState, change)

    let print state step =
        printfn "%d" step
        state 
        |> Array.iter (fun row -> 
            row |> Array.iter (printf "%c")
            printfn ""
            )

    let rec solve state flips =
    //    print state flips
        let (s2, changed) = flipAll state
        if (changed) then solve s2 (flips + 1)
        else (flips, state)
        
    let (num, final) = solve seats 1

    printfn "After %d the final state is" num
    print final num

    final
    |> Array.sumBy (fun row -> row |> Array.sumBy (fun x -> if x = OCCUPIED then 1 else 0))
    |> printfn "found %d occupied"
    
    0 // return an integer exit code
