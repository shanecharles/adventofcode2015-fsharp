let move (x,y) = function
    | '^' -> (x, y + 1)
    | 'v' -> (x, y - 1)
    | '<' -> (x - 1, y)
    | '>' -> (x + 1, y)
    | _   -> (x, y)

let countHouses dirs = 
    dirs 
    |> Seq.fold (fun (p,(s : (int * int) Set)) d -> 
                    let p' = move p d
                    (p', (s.Add p'))) ((0,0), Set.ofList [(0,0)])
    |> fun (_, s) -> s |> Seq.length

let countHouses' dirs = 
    dirs 
    |> Seq.fold (fun (p1, p2, (s : (int * int) Set)) d -> 
                    let p' = move p1 d
                    (p2, p', (s.Add p'))) ((0,0), (0,0), Set.ofList [(0,0)])
    |> fun (_, _, s) -> s |> Seq.length

let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\input3.txt")