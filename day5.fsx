
let niceString input =
    let vs = Set.ofList ['a'; 'e'; 'i'; 'o'; 'u']
    let ns = Set.ofList [[|'a';'b'|]; [|'c';'d'|]; [|'p';'q'|]; [|'x';'y'|]]
    let mv : string -> bool = Seq.filter (fun c -> vs.Contains(c)) >> Seq.length >> (<=) 3
    let cc : string -> bool = Seq.windowed 2 >> Seq.map (fun [|x ; x'|] -> x = x') >> Seq.exists id
    let ns : string -> bool = Seq.windowed 2 >> Seq.exists (fun ca -> ns.Contains(ca))
    input |> mv && input |> cc && input |> ns |> not

let niceString' input =
    let removeTrips s = 
        s |> Seq.fold 
                    (fun (next : string) c -> 
                                next.Replace(String.replicate 3 c, String.replicate 2 c)) input

    let xyx = Seq.windowed 3 >> Seq.exists (fun [|a; _; c|] -> a = c)
    let sanitized = input |> Seq.windowed 3
                  |> Seq.choose (function
                            | [| a; b; c |] when a = b && b = c -> Some (a.ToString())
                            | a             -> None)
                  |> Seq.distinct
                  |> removeTrips
    
    let pairs = sanitized |> Seq.windowed 2 
    let unique = pairs |> Seq.distinct
    unique |> Seq.map (fun u -> pairs |> Seq.filter (fun p -> p = u) |> Seq.length)
    |> Seq.exists (fun c -> c >= 2)
    && input |> xyx
    
type Kid = 
    | Naughty
    | Nice

let (|IsNiceKid|_|) (input : string) =
    if input |> niceString then Some ()
    else None

let NaughtyOrNice = function
    | IsNiceKid -> Nice
    | _         -> Naughty

let (|IsNiceKid'|_|) (input : string) =
    if input |> niceString' then Some ()
    else None

let NaughtyOrNice' = function
    | IsNiceKid' -> Nice
    | _         -> Naughty
    
let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input5.txt")

let NiceCount f = Seq.map f >> Seq.filter (fun n -> n = Nice)
                  >> Seq.length

input |> NiceCount NaughtyOrNice |> printfn "Day 5 part 1: %d"
input |> NiceCount NaughtyOrNice' |> printfn "Day 5 part 2: %d"