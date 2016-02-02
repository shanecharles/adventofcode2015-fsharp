open System
open System.Text
let file = __SOURCE_DIRECTORY__ + @"\input12.txt"
let input = IO.File.ReadAllText file

let sum (input : string) = 
    input.Split ([|'['; ']'; ','; '{';'}'; ':'|])
    |> Seq.filter (fun s -> not (s.StartsWith("\"") || s.Trim() |> String.IsNullOrEmpty))
    |> Seq.map Int32.Parse
    |> Seq.sum

let part1 = sum input

let pattern = ":\"red\""
let results = RegularExpressions.Regex.Matches(input, pattern)

let findBounds input idx =
    let input' = input |> Seq.toArray
    let rec findEndPoint (input : char []) (c1, c2) dir (idx,acc) = 
        match acc with 
        | 0 -> idx
        | a ->
            let idx' = dir idx
            match input.[idx'] with 
            | c when c1 = c -> findEndPoint input (c1, c2) dir (idx',(acc - 1))
            | c when c2 = c -> findEndPoint input (c1, c2) dir (idx',(acc + 1))
            | _             -> findEndPoint input (c1, c2) dir (idx',acc)

    let fStart = findEndPoint input' ('{', '}') (fun i -> i - 1) (idx,1)
    let fEnd = findEndPoint input' ('}','{') ((+) 1) (idx,1)
    fStart, fEnd

let fb = findBounds input

let bounds = [ for m in results do 
                    yield m.Index |> fb ]

let numbers = RegularExpressions.Regex.Matches(input, "([-0-9]+)")
let part1' = [for n in numbers -> Int32.Parse(n.Value) ]
             |> List.sum

let part2 = numbers 
            |> Seq.cast<RegularExpressions.Match>
            |> Seq.map (fun n -> 
                            match bounds |> List.exists (fun (s,e) -> s < n.Index && n.Index < e) with
                            | true -> 0
                            | _    -> Int32.Parse n.Value)
            |> Seq.sum