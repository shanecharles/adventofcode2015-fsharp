
let test = "(())"

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

let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\input1.txt") 
let floor = input |> elevator
let first = input |> firstBasement