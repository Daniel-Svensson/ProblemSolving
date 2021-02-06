// Learn more about F# at http://fsharp.org

open System
open System.IO

type Operation =
    | Nop of int
    | Acc of int
    | Jmp of int


let strToOperation (str:string) =
    match str.[0] with
    | 'n' -> Nop (Int32.Parse str.[3..]) 
    | 'a' -> Acc (Int32.Parse str.[3..]) 
    | 'j' -> Jmp (Int32.Parse str.[3..])
    | _ -> failwith "invalid operator"


let evaluate (program : Operation array) =
    let visited = Array.zeroCreate<bool> program.Length

    let rec run i acc = 
        if (i = program.Length) then Ok acc
        else if i < 0 || i > program.Length then Error (i, acc) 
        else if visited.[i] then Error (i, acc)
        else 
            visited.[i] <- true
            match program.[i] with
            | Nop _ -> run (i+1) acc
            | Acc change -> run (i+1) (acc + change)
            | Jmp steps -> run (i + steps) acc

    run 0 0

let checkSubstitution (program : Operation array) idx =
    let substituteAndEvaluate newOperation =
        let copy = Array.copy program
        copy.[idx] <- newOperation
        match evaluate copy with
        | Ok acc -> Some acc
        | Error _ -> None

    match program.[idx] with
    | Acc _ -> None
    | Nop i -> substituteAndEvaluate (Jmp i)
    | Jmp i -> substituteAndEvaluate (Nop i)


[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"

    let program = 
        File.ReadAllLines "input.txt"
        |> Array.map strToOperation
    
    program
    |> evaluate
    |> printfn "The result (duplicate id, accumulator) is %A"

    program
    |> Array.mapi (fun i x -> checkSubstitution program i)
    |> Array.choose id
    |> printfn "Correct substitution gives %A"

    

    0 // return an integer exit code
