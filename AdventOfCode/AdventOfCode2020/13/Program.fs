// Learn more about F# at http://fsharp.org

open System
open Flips
open Flips.Types


type Bus = {Id:int; WaitTime:int}
type Bus2 = { Idx : int64; Buss : int64 }

[<EntryPoint>]
let main argv =
    let input = IO.File.ReadAllLines "input.txt"
    let start = input.[0] |> Int32.Parse

    let waitTime id =
        let multiples = start / id + 1
        let pickupTime = multiples * id
        pickupTime - start

    input.[1].Split(",")
    |> Array.choose (fun x -> 
        match Int32.TryParse x with 
        | (true, id) -> Some {Id = id; WaitTime = (waitTime id)}
        | _ -> None)
    |> Array.minBy (fun x -> x.WaitTime)
    |> (fun bus -> 
        printfn "%A with val %d" bus (bus.WaitTime * bus.Id)
        )

    // -------------------  PART 2 -------------------
    (**
    let t = Decision.createInteger "t" 0.0 infinity
    let goal = Objective.create "first time" Types.Minimize (t * 1.0)

    let constraints = 
        input.[1].Split(",")
        |> Array.mapi (fun idx x ->
            match Int32.TryParse x with 
            | (true, id) -> Some (idx, id)
            | _ -> None)
        |> Array.choose id
        |> Array.map(fun (idx, id) ->
            let bustimes = Decision.createInteger $"bus{idx}_{id}" 0.1 infinity
            Constraint.create $"bus_{idx}" (t + (float idx) == bustimes * (float id)))             

    let model =
        Model.create goal
        |> Model.addConstraints constraints

    let result = Solver.solve Settings.basic model

    printfn "-- Result from Flips --"
    printfn "Res %A" result
    **)
    
    let rec gcd (a:int64) b =
        let r = a % b
        if r = LanguagePrimitives.GenericZero then b
        else gcd b r

    // least common multiple, gives numer of iterations between combination start repeating themselves
    let lcm a b =
        (a / (gcd a b)) * b

  
    let parse (text:string) = 
        text.Split(',')
        |> Array.mapi (fun idx x ->
            match Int64.TryParse x with 
            | (true, id) -> Some {Idx = (int64 idx); Buss = id}
            | _ -> None)
        |> Array.choose id
        |> Array.sortByDescending (fun x -> x.Buss)

    // Given a time, and stepsize  (bus interval) and another bus b 
    // gives the first time (larger than t or equal to t) satisfying the south condition
    // where (the bus b arrives at t + b.idx and where all previous busses arrives as  
    // new t is t + a*stepSize
    // new stepsize is lcm of current and the bus
    let rec combine (t, stepSize) (b:Bus2) =
        if ((t + b.Idx) % b.Buss = LanguagePrimitives.GenericZero) then (t, lcm stepSize b.Buss)
        else combine ((t + stepSize), stepSize) b

    let solve text = 
        let numbers = parse text
        numbers.[1 ..]
        |> Array.fold combine (numbers.[0].Buss - numbers.[0].Idx, numbers.[0].Buss)
        |> printfn "Solution %A"

    solve input.[1]
  
    0 // return an integer exit code
