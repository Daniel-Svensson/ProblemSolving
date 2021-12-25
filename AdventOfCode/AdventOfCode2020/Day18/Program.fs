// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO
//open System.Collections.Generic

type Token =
    Number of int64
    | Plus
    | Mult
    | OpenParen
    | CloseParen

let tokenize (str:string) =
    str.ToCharArray()
    |> Array.choose (fun ch ->
        match ch with
        | '+' -> Some Plus
        | '*' -> Some Mult
        | '(' -> Some OpenParen
        | ')' -> Some CloseParen
        | ch when Char.IsDigit(ch) ->
            Some (Number (int64 ch - int64 '0'))
        | ' ' -> None
        | _ -> failwith "invalid")
    |> Array.toList

let eval (exp:List<Token>) =
    let mutable current = exp

    let readSym() =
        if (List.isEmpty current) then CloseParen
        else 
            let sym = List.head current
            current <- List.tail current
            sym

    // Read complete expression (anything between parentesis)
    let rec readExpre() =
        let rec step acc =
            match readSym() with
            | CloseParen -> acc
            | Plus -> step (acc + readNum())
            | Mult -> step (acc * readNum())
            | sym -> failwithf "Unexpected sumbol %A" sym 
        step (readNum())
    and readNum() = 
        match readSym() with 
        | OpenParen  -> readExpre() 
        | Number i -> i
        | _ -> failwith "expected number"

    readExpre ()



// Shunting yard algorithm

//while there are tokens to be read:
//read a token
//if the token is:
//- a number:
//    put it into the output queue
//- a function:
//    push it onto the operator stack 
//- an operator o1:
//    while (
//        there is an operator o2 other than the left parenthesis at the top
//        of the operator stack, and (o2 has greater precedence than o1
//        or they have the same precedence and o1 is left-associative)
//    ):
//        pop o2 from the operator stack into the output queue
//    push o1 onto the operator stack
//- a left parenthesis (i.e. "("):
//    push it onto the operator stack
//- a right parenthesis (i.e. ")"):
//    while the operator at the top of the operator stack is not a left parenthesis:
//        {assert the operator stack is not empty}
//        /* If the stack runs out without finding a left parenthesis, then there are mismatched parentheses. */
//        pop the operator from the operator stack into the output queue
//    {assert there is a left parenthesis at the top of the operator stack}
//    pop the left parenthesis from the operator stack and discard it
//    if there is a function token at the top of the operator stack, then:
//        pop the function from the operator stack into the output queue
///* After the while loop, pop the remaining items from the operator stack into the output queue. */
//while there are tokens on the operator stack:
///* If the operator token on the top of the stack is a parenthesis, then there are mismatched parentheses. */
//{assert the operator on top of the stack is not a (left) parenthesis}
//pop the operator from the operator stack onto the output queue

type 'T Stack = List<'T>
module Stack =
    let empty = List.empty
    let push n s = n :: s
    let pop s = (List.head s, List.tail s)
    let peek s = List.head s
    let drop s = List.tail s
    let isEmpty s = List.isEmpty s

let shuntingYard (tokens:List<Token>) =
    let rec impl t (numbers:Stack<int64>) (operators:Stack<Token>) =
        if (List.isEmpty t)
        then
            let (newNumbers, _) = evalStack (fun _ -> true) numbers operators
            match newNumbers with
            | [x] -> x
            | _ -> failwith "operandstack error"
        else
            let tail = List.tail t
            match (List.head t) with
            | Number n -> 
                impl tail (Stack.push n numbers) operators
            | Plus -> 
                let (newNums, newOps) = evalStack (fun x -> x = Plus) numbers operators
                impl tail newNums (Stack.push Plus newOps)
            | Mult -> 
                let (newNums, newOps) = evalStack (fun x -> x = Plus || x = Mult) numbers operators
                impl tail newNums (Stack.push Mult newOps)
            | OpenParen -> 
                impl tail numbers (Stack.push OpenParen operators)
            | CloseParen -> 
                let (newNums, newOps) = evalStack (fun x -> true) numbers operators
                impl tail newNums newOps
    and evalStack f num op = 
        if ((Stack.isEmpty op) || (not (f (Stack.peek op))))
            then num, op
        else
            match num, (Stack.pop op) with
            | a :: b :: nums , (Plus, ops)-> evalStack f ((a + b) :: nums) ops 
            | a :: b :: nums , (Mult, ops)-> evalStack f ((a * b) :: nums) ops
            | nums , (OpenParen, ops) -> nums, ops
            | _ -> failwith "unexpected"

    impl tokens Stack.empty Stack.empty


[<EntryPoint>]
let main argv =

    let values = File.ReadAllLines "input.txt" |> Array.map (tokenize >> eval)

    printfn "Values: %A" values

    printfn "Sum is %d" (Array.sum values)

    let values2 = File.ReadAllLines "input.txt" |> Array.map (tokenize >> shuntingYard)
    
    printfn "Values: %A" values2
    
    printfn "Sum is %d" (Array.sum values2)
    

    0 // return an integer exit code