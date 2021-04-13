// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

type T = uint64
type BitMask = {AndMask:T ; OrMask:T}
type State = {BitMask : BitMask; Values : Map<T, T>}

type Operation = 
    | SetMask of BitMask
    | SetValue of Address:T * Value:T

let parseBitMask (text:string) =
    let mutable orMask = 0UL
    let mutable andMask = 0xffffffffffffffffUL
    text 
    |> Seq.iteri (fun i c ->
        match c with 
            | '1' -> 
                orMask <- orMask ||| (1UL <<< 35 - i)
            | '0' -> 
                andMask <- andMask  ^^^ (1UL <<< 35 - i)
            | 'X'  -> ()
            | _ -> failwith "Invalid mask")
    SetMask {AndMask = andMask; OrMask = orMask}

let maskPrefix = "mask = ";

let parse (text:string) = 
    if text.StartsWith(maskPrefix) 
    then parseBitMask text.[maskPrefix.Length..]
    else
        let startBracket = text.IndexOf '['
        let endBracket = text.IndexOf ']'
        let assign = text.IndexOf "= "
        let address = text.[startBracket+1 .. endBracket-1] |> uint64
        let value = text.[assign+2..] |> uint64
        SetValue (address, value)

let eval state operation = 
    match operation with
    | SetMask bitmask ->
        {state with BitMask = bitmask}
    | SetValue (address, value) ->
        let actualValue = (value &&& state.BitMask.AndMask) ||| state.BitMask.OrMask
        {state with Values = Map.add address actualValue state.Values}



let sum state = 
    state.Values 
    |> Map.fold (fun acc key value -> acc + value) 0UL

[<EntryPoint>]
let main argv =
    
    IO.File.ReadAllLines "input.txt"
    |> Array.map parse
    |> Array.fold eval {BitMask = {AndMask=0UL; OrMask=(uint64 -1L)}; Values = Map.empty}
    |> sum
    |> printfn "Sum %A"


    0 // return an integer exit code