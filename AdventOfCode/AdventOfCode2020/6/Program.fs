// Learn more about F# at http://fsharp.org

open System
open System.IO

type Reply = {Size : int; Ansver : Set<Char> }


let parse (lines:string array) =
    let mutable count = 0
    let mutable items = Set.empty

    seq {
        for line in lines do
            if (String.IsNullOrWhiteSpace line) then
                yield {Size = count; Ansver = items}
                count <- 0
                items <- Set.empty
            else
                count <- count + 1
                for ch in line do
                    items <- Set.add ch items
        
        if (count > 0) then
            yield {Size = count; Ansver = items}
    }
    |> Seq.toArray


let parseRecords f (lines:string array) =
    let items = ResizeArray()
    [|
        for line in lines do
            if (String.IsNullOrWhiteSpace line) then
                yield items.ToArray()
                items.Clear()
            else
                items.Add(f line)
        
        if (items.Count > 0) then
            yield items.ToArray()
    |]

let toSet (str:string) = Set.ofSeq str
    

let numAnsvers reply =
    Set.count reply.Ansver

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"

    let input = File.ReadAllLines "input.txt"

    let answers = parse input

    printfn "%A" answers

    answers
    |> Seq.sumBy numAnsvers
    |> printfn "%d "

    answers 
    |> Seq.map numAnsvers
    |> printfn "%A"

    parseRecords toSet input
    |> Array.map (fun record -> 
        record
        |> Array.fold Set.union record.[0])
    |> Array.sumBy Set.count
    |> printfn "total sum is %d"


    parseRecords toSet input
      |> Array.map (fun record -> 
          record
          |> Array.fold Set.intersect record.[0])
      |> Array.sumBy Set.count
      |> printfn "total sum is %d"

    0 // return an integer exit code
