// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// Define a function to construct a message to print
[<Struct>]
type Range = {Min:int; Max:int}

[<Struct>]
type Rule = {Name: string; Range1: Range; Range2: Range}

type Ticket = int[]

module Rule =
    let parse (str:string) =
        let parseRange (s:string) =
            let parts = s.Split('-')
            {Min = parts.[0] |> int ; Max = parts.[1] |> int}

        let nameAndRanges = str.Split(": ")
        let ranges = nameAndRanges.[1].Split(" or ")
        {Name = nameAndRanges.[0]; Range1 = (parseRange ranges.[0]); Range2 = (parseRange ranges.[1])}

    let inRange i range =
        range.Min <= i && i <= range.Max

    let valid i rule =
        (inRange i rule.Range1) || (inRange i rule.Range2)

module Ticket =
    let parse (str:string) =
        str.Split(',')
        |> Array.map int

type ProblemInput = {Rules: Rule[]; YourTicket: Ticket; NearbyTickets: Ticket[]}

let parse (lines:string[]) =
    let mutable i = 0

    let rules = 
        lines 
        |> Array.takeWhile (not << String.IsNullOrEmpty)
        |> Array.map Rule.parse

    let tickets = 
        lines
        |> Array.skip (rules.Length + 5)
        |> Array.map Ticket.parse
    {
    Rules = rules
    YourTicket = Ticket.parse lines.[rules.Length + 2]
    NearbyTickets = tickets
    }


let completelyInvalid rules ticket = 
    ticket
    |> Array.forall (fun i -> rules |> Array.forall (fun r -> not (Rule.valid i r)))

[<EntryPoint>]
let main argv =

    let input = IO.File.ReadAllLines "input.txt"

    let problem = parse input

    let ticketSum rules ticket = 
        ticket |> 
        Array.sumBy (fun i -> 
            if (Array.forall (not << (Rule.valid i)) rules) then i
            else 0
        )

    problem.NearbyTickets
    |> Array.sumBy (ticketSum problem.Rules)
    |> printfn "Sum is %d"


    0 // return an integer exit code