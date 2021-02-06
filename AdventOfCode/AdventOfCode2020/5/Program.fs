// Learn more about F# at http://fsharp.org

open System
open System.IO

type Seat = { Row: int; Column: int; SeatId : int}

let lowMid min max = (min + max) / 2 
let hiMid min max = (min + max + 1) / 2

let findNumber lower upper str min max =
    let rec impl index min max = 
        if index = String.length str then assert (min = max); min
        else if str.[index] = lower then impl (index+1) min (lowMid min max)
        else if str.[index] = upper then impl (index+1) (hiMid min max) max
        else failwith "invalid char"
    impl 0 min max

let parseSeat (str:string) =
    let row = findNumber 'F' 'B' str.[..6] 0 127
    let col = findNumber 'L' 'R' str.[7..] 0 7
    { Row = row; Column = col; SeatId = row * 8 + col }

let seatId seat = seat.SeatId

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"

    let input = File.ReadAllLines("input.txt")
    
    let passes = input |> Array.map parseSeat

    passes
    //|> Seq.iter (printfn "seat: %A")
//    |> Seq.map seatId
    |> Seq.maxBy (fun x -> x.SeatId)
    |> printfn "Max seat is %A" 

    
    Array.sortInPlaceBy seatId passes

    //  Find seat 
    for i in 1..passes.Length-2 do
        if (passes.[i].SeatId + 2 = passes.[i+1].SeatId) then
            printfn "Seat %A with next seat %A" passes.[i] passes.[i+1]
            printfn "Your seat is %d" (passes.[i].SeatId + 1) 

    0 // return an integer exit code
