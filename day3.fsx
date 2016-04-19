let move (x,y) = function
    | '^' -> (x, y + 1)
    | 'v' -> (x, y - 1)
    | '<' -> (x - 1, y)
    | '>' -> (x + 1, y)
    | _   -> (x, y)
let part1 = Seq.scan move (0,0) >> Seq.distinct >> Seq.length
let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/input3.txt")
input |> part1 |> printfn "Day 3 part 1: %d"
type Delivery =
  | Santa
  | Robo
let whoDelivers x = if x % 2 = 0 then Santa else Robo

input |> Seq.mapi (fun i x -> (whoDelivers i, x))
      |> Seq.fold 
        (fun (s,r) (w,x) -> match (w,s,r) with
                            | (Santa, h::_, _) -> ((move h x) :: s, r)
                            | (Robo, _, h::_)  -> (s, (move h x) :: r))
        ([(0,0)],[(0,0)])
      |> (fun (s,r) -> s @ r)
      |> Seq.distinct
      |> Seq.length
      |> printfn "Day 3 part 2: %d"
 
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

