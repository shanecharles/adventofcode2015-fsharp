open System

let input = __SOURCE_DIRECTORY__ + @"/input17.txt"
let containers =
  System.IO.File.ReadAllLines(input) 
  |> Array.filter (fun l -> l |> String.IsNullOrEmpty |> not)
  |> Array.map Int32.Parse
  |> List.ofArray

let combinations cs = 
  let rec combos n l = 
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, hd::tl -> List.map ((@) [hd]) (combos (k-1) tl) @ combos k tl
  [1 .. cs |> Seq.length] |> Seq.map (fun n -> combos n cs) |> Seq.concat

let part1 = containers |> combinations |> Seq.filter (fun s -> s |> Seq.sum = 150) |> Seq.length
let part2 = containers |> combinations 
            |> Seq.filter (fun s -> s |> Seq.sum = 150)
            |> Seq.map (fun s -> s |> Seq.length)
            |> Seq.sort
            |> Seq.groupBy id
            |> Seq.head
            |> snd
            |> Seq.length
            