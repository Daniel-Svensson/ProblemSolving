open System.IO


let publicKeys = File.ReadAllLines "input.txt" |> Array.map uint64

let findLoopSize2 subjectNumber target =
    let rec impl i value = 
        if (value = target) then i
        else impl (i + 1) ((value * subjectNumber) % 20201227UL)
    impl 1 subjectNumber

let loopSizes = publicKeys |> Array.map (findLoopSize2 7UL)


printfn "loop sizes %A" loopSizes

let transform subjectnumber loopsize =
    let mutable value = 1UL
    for i in 1..loopsize do
        value <- (value * subjectnumber) % 20201227UL
    value

let key = transform publicKeys[0] loopSizes[1]
let key2 = transform publicKeys[0] loopSizes[1]

printfn $"Key is {key} {key2}"
