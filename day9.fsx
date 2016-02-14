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
            |> Array.toList

let removeDest dest = List.filter (fun {To=t} -> t <> dest)

let shortest = List.sortBy (fun {Weight=w} -> w)
let longest = List.sortByDescending (fun {Weight=w} -> w)

let createPath fnsort es = 
    let rec path acc rs = 
        match rs with 
        | [] -> acc
        | _  -> 
            match acc with 
            | []     -> 
                let dest = rs |> fnsort |> List.head
                path (dest::acc) (rs |> removeDest dest.To |> removeDest dest.From) 
            | h :: _ ->
                printfn "%s -> %s" h.From h.To
                let dest = rs |> List.filter (fun {From=fr} -> fr = h.To)
                           |> fnsort
                           |> List.head
                path (dest :: acc) (rs |> removeDest dest.To) 
    es |> List.map (fun {From = f} -> f)
    |> List.distinct
    |> List.map (fun city -> es |> List.filter (fun {From=f} -> f = city) |> fnsort |> List.head)
    |> List.map (fun dest -> path [dest] (es |> removeDest dest.To |> removeDest dest.From))

let calcWeight = List.map (fun {Weight=w} -> w) >> List.sum

let part1 = edges |> createPath shortest |> List.map calcWeight |> List.min
let part2 = edges |> createPath longest |> List.map calcWeight |> List.max