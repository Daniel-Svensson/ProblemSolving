// Learn more about F# at http://fsharp.org

open System
open System.IO
open FParsec



let input = File.ReadAllLines("input.txt")

(*byr (Birth Year)
iyr (Issue Year)
eyr (Expiration Year)
hgt (Height)
hcl (Hair Color)
ecl (Eye Color)
pid (Passport ID)
cid (Country ID)
*)

let yearInRange str min max =
    String.length str = 4
    && match Int32.TryParse str with
        | false, _ -> false
        | true, value -> min <= value && value <= max

let validHeight (str:string) = 
    let numbers = str.[..(str.Length - 3)]
    let measure = str.[str.Length - 2 ..]
    match (Int32.TryParse numbers), measure with
    | (true, cm), "cm" -> 150 <= cm && cm <= 193
    | (true, inch), "in" -> 59 <= inch && inch <= 76
    | (_, _), _ -> false

let isHex (ch) =
    '0' <= ch && ch <= '9' 
    || 'a' <= ch && ch <= 'f'

let validHairColor (str:string) =
    String.length str = 7
    && str.[0] = '#'
    && String.forall isHex str.[1..]

let eyecolors = [|"amb";"blu"; "brn"; "gry"; "grn"; "hzl"; "oth"|]
let validEyeColor str = 
    eyecolors
    |> Array.contains str

let validPassportId str =
    String.length str = 9
    && String.forall Char.IsDigit str


type Passport = {Fields : Map<string,string>}

let parsePassports (lines:string array) =
    let mutable fields = Map.empty
    seq {
        for line in lines do
            let pairs = line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
            if Array.isEmpty pairs then 
                yield {Fields = fields}
                fields <- Map.empty
            else
                for pair in pairs do
                    let keyvalue = pair.Split(':')
                    fields <- Map.add keyvalue.[0] keyvalue.[1] fields

        yield {Fields = fields}
    }        


let requriedFields = [|"byr"; "iyr";"eyr";"hgt";"hcl";"ecl";"pid";|]

let isValid reqFields passport =
    Seq.forall passport.Fields.ContainsKey reqFields
    && yearInRange passport.Fields.["byr"] 1920 2002
    && yearInRange passport.Fields.["iyr"] 2010 2020
    && yearInRange passport.Fields.["eyr"] 2020 2030
    && validHeight passport.Fields.["hgt"]
    && validHairColor passport.Fields.["hcl"]
    && validEyeColor passport.Fields.["ecl"]
    && validPassportId passport.Fields.["pid"]

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"

    input 
    |> parsePassports
    |> Seq.filter (isValid requriedFields)
    |> Seq.sumBy (fun x -> 1)
    |> printfn "Valid count is %d"

    0 // return an integer exit code
