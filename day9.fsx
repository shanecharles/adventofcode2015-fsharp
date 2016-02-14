open System
let file = __SOURCE_DIRECTORY__ + @"/input9.txt"

type Edge = { From : string; To : string; Weight : int }
let parseEdge (l : string) = 
    l.Split([|" to "; " = "|], StringSplitOptions.None)
    |> fun [| a; b; d |] -> 
        [| {From = a; To = b; Weight = Int32.Parse(d)}; 
           {From = b; To = a; Weight = Int32.Parse(d)} |]

let edges = file |> IO.File.ReadAllLines 
            |> Array.filter (String.IsNullOrEmpty >> not)
            |> Array.map parseEdge
            |> Array.concat
            |> Array.sortBy (fun {Weight=w} -> w)
            |> Array.toList

let removeDest dest = List.filter (fun {To=t} -> t <> dest)

let createPath es = 
    let rec path acc rs = 
        match rs with 
        | [] -> acc
        | _  -> 
            match acc with 
            | []     -> 
                let dest = List.head rs
                path (dest::acc) (rs |> removeDest dest.To |> removeDest dest.From) 
            | h :: _ ->
                printfn "%s -> %s" h.From h.To
                let dest = rs |> List.filter (fun {From=fr} -> fr = h.To)
                           |> List.sortBy (fun {Weight=w} -> w)
                           |> List.head
                path (dest :: acc) (rs |> removeDest dest.To) 
    path [] es

let part1 = edges |> createPath |> List.map (fun {Weight = w} -> w) |> List.sum;;