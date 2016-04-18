open System.IO
let input = File.ReadAllText(__SOURCE_DIRECTORY__ + @"/input1.txt") 
let move x = function
             | '(' -> x + 1
             | ')' -> x - 1
             | _   -> failwith "Invalid char"
let part1 = input |> Seq.fold move 0 |> printfn "Day 1 part 1: %d"
let part2 = input |> Seq.scan move 0 |> Seq.takeWhile ((<>) -1)
            |> Seq.length
            |> printfn "Day 1 part 2: %d"
(*
let elevator : string -> int = 
    Seq.fold (fun floor dir -> 
                    match dir with 
                    | '(' -> floor + 1
                    | ')' -> floor - 1
                    | _   -> floor) 0

let firstBasement : string -> int = 
    Seq.fold (fun (f, i, p) dir -> 
                    let f' = match dir with 
                             | '(' -> f + 1
                             | ')' -> f - 1
                             | _   -> f
                    let p' = match f', p with
                             | -1, 0 -> i
                             | _, _  -> p
                    (f', (i + 1), p')) (0, 1, 0)
    >> function (_, _, pos) -> pos

let floor = input |> elevator
let first = input |> firstBasement
*)
