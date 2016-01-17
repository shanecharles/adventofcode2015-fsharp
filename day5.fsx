
let niceString input =
    let vs = Set.ofList ['a'; 'e'; 'i'; 'o'; 'u']
    let ns = Set.ofList [[|'a';'b'|]; [|'c';'d'|]; [|'p';'q'|]; [|'x';'y'|]]
    let mv : string -> bool = Seq.filter (fun c -> vs.Contains(c)) >> Seq.length >> (<=) 3
    let cc : string -> bool = Seq.windowed 2 >> Seq.map (fun [|x ; x'|] -> x = x') >> Seq.exists id
    let ns : string -> bool = Seq.windowed 2 >> Seq.exists (fun ca -> ns.Contains(ca))
    input |> mv && input |> cc && input |> ns |> not

type Kid = 
    | Naughty
    | Nice

let (|IsNiceKid|_|) (input : string) =
    if input |> niceString then Some ()
    else None

let NaughtyOrNice = function
    | IsNiceKid -> Nice
    | _         -> Naughty
    
let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\input5.txt")

let niceKids = input |> Array.map NaughtyOrNice
               |> Array.filter (fun n -> n = Nice)
               |> Array.length