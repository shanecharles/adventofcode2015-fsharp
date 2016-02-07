open System

let raw = IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\input16.txt")

type Sue = { Id : int; Attributes : (string * string) Set }
let parseAtts (line : string) = 
    line.Split([|','|])
    |> Array.map (fun a -> a.Split([|':'|]) 
                           |> fun v -> (v.[0].Trim(), v.[1].Trim()))
    |> Set.ofArray

let createSue id (line : string) = 
    let atts = line.Split([|id.ToString() + ":"|], StringSplitOptions.None) |> fun a -> a.[1] |> parseAtts
    {Id = id; Attributes = atts}

let sues = raw |> Array.filter (fun l -> not (l |> String.IsNullOrEmpty)) 
           |> Array.mapi (fun i l -> l |> createSue (i+1))

let attributes = [ ("children", "3");
                    ("cats","7");
                    ("samoyeds","2");
                    ("pomeranians","3");
                    ("akitas","0"); 
                    ("vizslas","0");
                    ("goldfish","5");
                    ("trees","3");
                    ("cars","2");
                    ("perfumes","1") ] |> Set.ofList

let part1 = sues |> Array.map ( fun {Id = i; Attributes = atts} -> (i, atts |> Set.intersect attributes |> Seq.length))
              |> Seq.maxBy snd