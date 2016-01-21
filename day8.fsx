let lines = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\input8.txt")

type Escape =
    | Escape
    | Control
    | Skip of int
    | Character

let chars (input : string) = 
    input |> Seq.fold (fun (acc, esc) c ->
                        match esc, c with 
                        | Skip n, _ -> (acc, n - 1 |> function 
                                                      | 0 -> Character
                                                      | n -> Skip n)
                        | Character, '\\' -> (acc, Escape)
                        | Escape, 'x'     -> (acc + 3, Skip 2)
                        | Escape, _       -> (acc + 1, Character)
                        | Character, '"'  -> (acc + 1, Character)
                        | _               -> (acc, Character)                 
                        ) (0, Character)
    |> fst
    |> fun c -> (input |> String.length) - c


let diff (input : string) = input.Length - (input |> chars)  

let calc input = input |> Seq.map diff |> Seq.sum

let t1 = """ "" """.Trim()
let t2 = """ "abc" """.Trim()
let t3 = """ "aaa\"aaa" """.Trim()
let t4 = """ "\x27" """.Trim()

let ts = [ t1; t2; t3; t4 ]

let result1 = lines |> calc