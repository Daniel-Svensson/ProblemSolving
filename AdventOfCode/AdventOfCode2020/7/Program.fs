// Learn more about F# at http://fsharp.org


open System
open System.IO
open FParsec

type Bag = { Name : string }
type InputRow = {Bag : string; Contents : list<int*string>}

let bagParser = 
    (charsTillString " bag" true 1000) 
        .>> optional (pchar 's')

let rowParser =
    pipe2 
        (bagParser .>> skipString " contain ")
        (sepBy (pint32 .>> spaces .>>. bagParser) (skipString ", ")
            .>> (optional (skipString "no other bags"))
            .>> (skipChar '.')
            .>> spaces)
        (fun bag contents -> 
            {Bag = bag; Contents= contents}
            )

let fileParser = 
    many rowParser

let readRow str =
    match run rowParser str with
    | Success (res,_,_) -> res
    | Failure (msg,_,_) -> failwith msg

let readFile path =
    match runParserOnFile fileParser () path Text.Encoding.Default with
    | Success (res,_,_) -> res
    | Failure (msg,_,_) -> failwith msg

[<Literal>]
let shinyGold = "shiny gold"

let rec canContainGold (map : Map<string, (int *string) list>) (contents: (int * string) list) =
     contents
     |> List.exists (fun (count, bag) -> 
         match (count, bag) with
         | _, "shiny gold" -> true
         | _ ,_ -> canContainGold map map.[bag])

[<EntryPoint>]
let main argv =
    let input = File.ReadAllLines("sample.txt")
    
  //  let rows1 =  input |> Array.map readRow
    let rows2 = readFile "input.txt"

    let map = rows2 
            |> List.map (fun row -> (row.Bag, row.Contents))
            |> Map.ofList


    let mutable count = 0
    for row in map do
        if row.Key <> shinyGold && canContainGold map row.Value then
            count <- 1 + count
            printfn "%s can contain gold" row.Key


    printfn "count is %d" count
            
    let rec countbagsNaive contents =
        1 + List.sumBy (fun (count, bag) -> count * (countbagsNaive map.[bag])) contents
    

    (countbagsNaive map.[shinyGold]) - 1
    |> printfn "Bag count is %d"



    printfn "Hello World from F#!"
    0 // return an integer exit code
