open System.Collections.Generic
open System.IO 
open System.Text
open System.Text.RegularExpressions

type Data = Dictionary<int, string[]>
type Messages = string[]
type Input = {Rules : Data; Messages : Messages}



let input = File.ReadAllLines "input.txt"
let rules = 
    input 
    |> Array.takeWhile ((<>) "") 
    |> Array.map (fun line -> 
        let parts = line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
        let number = parts.[0].Split(':').[0] |> int
        (number, parts[1..])
    )
    |> Map.ofArray
let messages = input.[ (rules.Count + 1) ..]


let cache = Dictionary<int, string>()

let rec solveRule idx = 
    match cache.TryGetValue idx with
    | (true, res) -> res
    | (false, _) ->
        let sb = StringBuilder("")
        let mutable hasOr = false
        for token in rules.[idx] do 
            if token.StartsWith('"') then sb.Append(token.[1 .. token.Length - 2])
            else if token = "|" then
                hasOr <- true
                sb.Append(token)
            else sb.Append(solveRule (int token))            
            |> ignore
        let mutable res = sb.ToString()
        if hasOr then
            res <- "(" + res + ")"
        cache.Add (idx, res)
        res

let regex =  new Regex ( "^" + (solveRule 0) + "$", RegexOptions.ExplicitCapture ||| RegexOptions.Compiled)
printfn "Regexes %A" regex
let messageIsMatch m = 
    regex.IsMatch(m)

let matches = messages |> Array.filter messageIsMatch
printfn "Match count is %d " matches.Length

printfn "\nPART 2--------------\n"

// overwrite 
cache.Clear()
// 8: 42 | 42 8  === one or many 42
cache.[8] <- $"({solveRule 42})+"

// 11: 42 31 | 42 11 31  , == balancing regex https://docs.microsoft.com/en-us/dotnet/standard/base-types/grouping-constructs-in-regular-expressions#balancing_group_definition
cache.[11] <- $"(?'Open'{solveRule 42})+(?'Close-Open'{solveRule 31})+(?(Open)(?!))"

let regex2 =  new Regex ("^" + (solveRule 0) + "$", RegexOptions.ExplicitCapture ||| RegexOptions.Compiled)

let matches2 = messages |> Array.filter regex2.IsMatch
printfn "Match count is %d " matches2.Length

