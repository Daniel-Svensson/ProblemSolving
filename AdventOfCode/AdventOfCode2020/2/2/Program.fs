// Learn more about F# at http://fsharp.org

open System
open System.IO
open FParsec

// Very inneficient solution
let isPasswordValid min max letter password =
    let count = password 
                |> String.filter (fun x -> x = letter)
                |> String.length
    min <= count && count <= max

// Policy 2 , valid if exactly one of the letters are the specified
let isPasswordValid2 i1 i2 letter (password:string) =
    match (letter = password.[i1 - 1], letter = password.[i2 - 1]) with
    | (true, false) -> true
    | (false, true) -> true
    | _ -> false

let parseRow =
    pipe4 pint32 
        (skipChar '-' >>. pint32) 
        (spaces >>. letter) 
        (skipChar ':' >>. spaces >>. manyChars letter)

(*** same as parseRow, trying more verbose CE version
let parseRow2 f = parse {
    let! min = pint32
    do! skipChar '-'
    let! max = pint32
    do! spaces
    let! ch = letter

    do! skipChar ':'
    do! spaces
    let! password = manyChars letter

    return f min max ch password
}
***)

let isRowValid isValid str =
    match run (parseRow isValid) str with
    | Success (res,_,_) -> printfn $"{str} : {res}"; res
    | Failure (errorMsg,_,_) -> failwith errorMsg

[<EntryPoint>]
let main argv =

    let validate policy passwords =
        passwords
        |> Array.sumBy (fun str -> if isRowValid policy str then 1 else 0)
        |> printfn "Count is %d"
 
    let passwords = File.ReadAllLines("input.txt")
 
    validate isPasswordValid passwords

    printfn "new policy"
    validate isPasswordValid2 passwords
          
    0 // return an integer exit code
