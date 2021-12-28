open System.IO
open System.Collections

let lines = File.ReadAllLines "input.txt"

let input = 
    lines
    |> Array.map (fun str -> 
        let idx = str.IndexOf("(contains ")
        {|
            Ingredients = str.[0 .. idx - 1].Split(' ', System.StringSplitOptions.RemoveEmptyEntries) |> Set.ofArray
            Allergents = str.[idx + "(contains ".Length .. str.Length - 2].Split(", ")
        |}
     )


let allergyIngredients = 
    input
    // Flatten by allergent
    |> Array.collect (fun row -> row.Allergents |> Array.map  (fun a -> (a, row.Ingredients) ))
    |> Array.groupBy (fun (allergent, _) -> allergent)
    |> Array.map (fun (key, arr) ->  (key, arr |> Array.map (fun (a, ing) -> ing) |> Set.intersectMany))

let allIngredients = 
    input 
    |> Array.fold (fun ingredients row -> Set.union row.Ingredients ingredients) Set.empty

printfn "Allergyingredients %A" allergyIngredients
printfn "All ingredients %A" allergyIngredients

let ingredientsWithoutAllergents = 
    allIngredients 
    |> Set.filter (fun ing -> not (Array.exists (fun (_, ingredients) -> Set.contains ing ingredients) allergyIngredients))

let numOccurrances = 
    input 
    |> Array.sumBy (fun row -> row.Ingredients |> Set.fold (fun acc ing -> if (Set.contains ing ingredientsWithoutAllergents) then acc + 1  else acc) 0)

printfn "No allergents %A occurs %d" ingredientsWithoutAllergents numOccurrances


// Map allergens and ingredients
let map = ResizeArray()
while (map.Count < allergyIngredients.Length) do
    let easyMappings = allergyIngredients |> Array.choose (fun (a,ing) -> if ing.Count = 1 then Some (a, Set.minElement ing) else None)
    map.AddRange (easyMappings)
    for (_, ingredient) in easyMappings do
        for i in 0 .. allergyIngredients.Length - 1 do
            let (a,ing) = allergyIngredients.[i] 
            allergyIngredients.[i] <- (a, Set.remove ingredient ing)

let sortedMap = map.ToArray() |> Array.sortBy (fun (a,i) -> a)
printfn "Sorted %A" sortedMap

sortedMap |> Array.map snd |> String.concat "," |> printfn "Key is %s"