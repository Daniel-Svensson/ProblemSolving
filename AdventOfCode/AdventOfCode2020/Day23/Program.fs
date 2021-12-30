

let crabGame (allCups:int[]) (rounds:int) = 

    // Keep track of "next"
    let next = Array.zeroCreate (allCups.Length + 1)
    let mutable lastCup = allCups.[0] 
    for i in 1.. allCups.Length - 1 do 
        let current = allCups[i]
        next[lastCup] <- current
        lastCup <- current
    next.[lastCup] <- allCups.[0]

    let mutable current = allCups[0]
    for i in 1.. rounds do

        let a = next.[current]
        let b = next.[a]
        let c = next.[b]
        let d = next[c]

        // remove (a,b,c)
        next[current] <- d

        let mutable destination = current - 1
        while (destination = a || destination = b || destination = c || destination < 1) do 
            destination <- if (destination > 1) then destination - 1 else allCups.Length

        // insert (a,b,c)
        let rest = next[destination]
        next[destination] <- a
        next[c] <- rest

        current <- d
    next

//let input =  // "653427918" //"389125467"
   
let input = "653427918".ToCharArray() |> Array.map (fun x -> (int x) - (int '0'))

let resPart1 = crabGame input 100

let mutable p1 = resPart1[1]
while p1 <> 1 do
    printf "%d" p1
    p1 <- resPart1[p1]
printfn "\n"

let resPart2 = crabGame (Array.append input [| (input.Length + 1 |> int) .. 1000000 |]) 10000000


// For more information see https://aka.ms/fsharp-console-apps
printfn "\n-- final -- "

let a = resPart2[1]
let b = resPart2[a]

printfn $"{a}  * {b} = {(int64 a) * (int64 b)}"

