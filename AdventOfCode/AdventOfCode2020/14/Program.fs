// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

type T = uint64
type BitMask = {AndMask:T ; OrMask:T}
type State = {BitMask : BitMask; Values : Map<T, T>}

type Operation = 
    | SetMask of string
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
    {AndMask = andMask; OrMask = orMask}

let maskPrefix = "mask = ";

let parse (text:string) = 
    if text.StartsWith(maskPrefix) 
    then SetMask text.[maskPrefix.Length..]
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
        {state with BitMask = parseBitMask bitmask}
    | SetValue (address, value) ->
        let actualValue = (value &&& state.BitMask.AndMask) ||| state.BitMask.OrMask
        {state with Values = Map.add address actualValue state.Values}



let sum state = 
    state.Values 
    |> Map.fold (fun acc key value -> acc + value) 0UL


type Mem =
    | AddressNode of Mem * Mem
    | Value of uint64

let bittest (value:uint64) bit = 
    (1UL <<< bit) &&& value <> 0UL

module Memory =

    let Zero = [1..36] |> List.fold (fun tree idx -> AddressNode (tree, tree)) (Value 0UL)

    let Write (mask:string) address value mem =
        let rec impl idx mem =
            match mem with 
            | AddressNode (zero, one) ->
                match mask.[idx] with
                | 'X' -> 
                    if Object.ReferenceEquals (zero,one) then 
                        let tree = (impl (idx+1) zero)
                        AddressNode (tree, tree)
                    else 
                        AddressNode ((impl (idx+1) zero), (impl (idx+1) one))
                | '1' -> AddressNode (zero, (impl (idx+1) one))
                | '0' ->
                    if (bittest address (35-idx)) then AddressNode (zero, (impl (idx+1) one))
                    else AddressNode ((impl (idx+1) zero), one)
                | _ -> failwith "Invalid bitmask"
            | Value i when idx = 36 -> Value value
            | _ -> failwithf "Invalid tree" 
        impl 0 mem

    let rec Sum mem =
        match mem with 
        | AddressNode (zero, one) -> 
            if Object.ReferenceEquals(zero, one) then Checked.(*) 2UL (Sum zero)
            else Checked.(+) (Sum zero) (Sum one)
        | Value v -> v
   
type MemoryState = {BitMask: string; Memory: Mem }
let eval2 (state:MemoryState) operation = 
    match operation with
    | SetMask bitmask ->
        {state with BitMask = bitmask}
    | SetValue (address, value) ->
        {state with Memory = Memory.Write state.BitMask address value state.Memory}

[<EntryPoint>]
let main argv =
    
    let input = 
        IO.File.ReadAllLines "input.txt"
        |> Array.map parse

    input
    |> Array.fold eval {BitMask = {AndMask=0UL; OrMask=(uint64 -1L)}; Values = Map.empty}
    |> sum
    |> printfn "Sum %A"

    input
    |> Array.fold eval2 {BitMask = String.Empty; Memory = Memory.Zero}
    |> (fun state -> Memory.Sum state.Memory)
    |> printfn "Sum %A"

    0 // return an integer exit code