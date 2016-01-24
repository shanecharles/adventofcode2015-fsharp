open System
let eoc = byte 'z'

let increment (input : string) = 
    input |> Seq.toList
    |> List.rev
    |> List.fold (fun (acc, carry) c -> 
                    match carry + (byte c) with 
                    | x when x > eoc -> ('a' :: acc, 1uy)
                    | x              -> ( (char x) :: acc, 0uy)
                ) ([], 1uy)
    |> fst |> fun cs -> String.Join("", cs)

let hasConfusing : string -> bool = Seq.exists (fun c -> ['i'; 'o'; 'l'] |> List.exists (fun c' -> c' = c))
let hasStraight : string -> bool = Seq.windowed 3 >> Seq.exists (function | ([| a; _; c |] as input) -> input = [| a .. c |] | _ -> false)
let multipleUniquePairs : string -> bool = 
    Seq.fold (fun (acc, c) i -> if i = c then c :: acc, i else acc, i) ([], ' ')
    >> fst >> List.distinct >> List.length >> (<) 1

let success input = input |> hasConfusing |> not 
                    && input |> hasStraight
                    && input |> multipleUniquePairs

let input1 = "cqjxjnds"
let next input =
    Seq.unfold (fun pwd -> let pwd' = increment pwd
                           Some (pwd', pwd')) input
    |> Seq.filter (fun pwd -> success pwd)
    |> Seq.head

let result1 = input1 |> next
let result2 = result1 |> next