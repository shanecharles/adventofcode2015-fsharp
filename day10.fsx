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

let lookSay' : string -> string = 
    Seq.fold (fun acc x ->
            match acc with
            | (c,x') :: t when x' = x -> (c+1,x) :: t
            | _ -> (1,x) :: acc) []
    >> List.rev
    >> Seq.collect (fun (c,x) -> sprintf "%d%c" c x)
    >> function xs -> String.Join("",xs)

let run f input t = {1 .. t} |> Seq.fold (fun i _ -> i |> f) input |> Seq.length