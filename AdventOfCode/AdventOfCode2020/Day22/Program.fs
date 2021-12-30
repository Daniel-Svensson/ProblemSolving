open System.IO
open System.Collections.Generic
open System.Diagnostics
open System


let lines = File.ReadAllLines "input.txt"
let player1 = lines |> Seq.skip 1 |> Seq.takeWhile (fun s -> s <> "") |> Seq.map (int) |> Queue
let player2 = lines |> Array.skip (player1.Count + 3) |> Array.map (int) |> Queue

let bench f = 
    let sw = Stopwatch.StartNew()
    let bytes = System.GC.GetAllocatedBytesForCurrentThread()
    let res = f()
    let elapesed = sw.Elapsed
    printfn "elapsed %A bytes (%d)" elapesed (System.GC.GetAllocatedBytesForCurrentThread() - bytes)
    res

let combat (p1:Queue<int>) (p2:Queue<int>) = 
    // let rec combat p1 p1 = 
    while (p1.Count > 0 && p2.Count > 0) do
        let c1 = p1.Dequeue()
        let c2 = p2.Dequeue()
        if (c1 > c2) then 
            p1.Enqueue c1
            p1.Enqueue c2
        else
            p2.Enqueue c2
            p2.Enqueue c1
    if (p1.Count > 0) then p1 else p2

let winner = bench (fun () -> combat (Queue player1) (Queue player2))

let score (deck:IReadOnlyCollection<int>) = 
    deck |> Seq.mapi (fun idx i -> (deck.Count - idx) * i) |> Seq.sum
printfn "Score is %d" (score winner)

// recursive combat
type State = {Player1Cards : int[]; Player2Cards : int[]}
type Winner = Player1 | Player2

let emptySet() =
    let comparer = 
        { new IEqualityComparer<State> with
              member this.Equals(x, y) = 
                ReadOnlySpan<int>(x.Player1Cards).SequenceEqual(y.Player1Cards)
                && ReadOnlySpan<int>(x.Player2Cards).SequenceEqual(y.Player2Cards)
              member this.GetHashCode(obj) = 
                obj.Player1Cards.GetHashCode()
        }
    System.Collections.Generic.HashSet<State>(comparer)


let rec recursiveCombat state (set:HashSet<State>) round =

    //printfn "\n-- Round %d --" round
    //printfn "Player1's deck: %A" state.Player1Cards
    //printfn "Player2's deck: %A" state.Player2Cards
    if (set.Contains state) then
  //      printfn "GAME WON"
        (Player1, state)
    else 
        match state with
        | {Player1Cards = [||]} -> (Player2, state)
        | {Player2Cards = [||]} -> (Player1, state)
        | _ -> 
            let c1 = state.Player1Cards.[0]
            let c2 = state.Player2Cards.[0]
            let (d1, d2) = (state.Player1Cards.[1..], state.Player2Cards.[1..])
            let winner = 
                if (c1 <= d1.Length && c2 <= d2.Length) then //c1 >= Length - 1
                    fst (recursiveCombat {Player1Cards = state.Player1Cards.[1..c1]; Player2Cards = state.Player2Cards.[1..c2]} (emptySet()) 1)
                else
                    if (c1 > c2) then Player1 else Player2
            
//            printfn "%A wins round %d" winner round
            let newState = match winner with 
                | Player1 -> {Player1Cards = (Array.append d1 [|c1;c2 |]); Player2Cards = d2}
                | Player2 -> {Player1Cards = d1; Player2Cards = (Array.append d2 [|c2; c1 |])}
        
            set.Add(state) |> ignore
            recursiveCombat newState set (round + 1)

let (w, st) = bench (fun () -> recursiveCombat {Player1Cards = player1.ToArray(); Player2Cards = player2.ToArray()} (emptySet()) 1)

match w with 
| Player1 -> st.Player1Cards
| Player2 -> st.Player2Cards
|> score
|> printfn "Score for winner is %d"

printfn "State: %A" st
