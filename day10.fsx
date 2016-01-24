open System

let test = "3115"
let result = "132115"

let test1 = "111221"
let result1 = "312211"

let start = "1"
let one_iteration = "11"

let input = "3113322113"

let lookSay (input : string) = 
    let rec lookSay' acc (c, n) = function
        | [] -> sprintf "%s%d%c" acc c n
        | h :: t -> 
            if h = n then lookSay' acc (c+1,n) t
            else
                let acc' = sprintf "%s%d%c" acc c n
                lookSay' acc' (1,h) t
    match input |> Seq.toList with 
    | [] -> ""
    | h :: t -> lookSay' "" (1,h) t

let lookSay' (input : string) = 
    input |> Seq.fold (fun acc c -> 
                match acc with 
                | (c', n) :: t when c' = c -> (c, n + 1) :: t
                | _ -> (c, 1) :: acc) []
    |> List.rev
    |> List.map (fun (c, n) -> sprintf "%d%c" n c)
    |> function cs -> System.String.Join("",cs)

let run f input t = {1 .. t} |> Seq.fold (fun i _ -> i |> f) input |> Seq.length