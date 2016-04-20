let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/input3.txt")
let move (x,y) = function
    | '^' -> (x, y + 1)
    | 'v' -> (x, y - 1)
    | '<' -> (x - 1, y)
    | '>' -> (x + 1, y)
    | _   -> (x, y)

let part1 = Seq.scan move (0,0) >> Seq.distinct >> Seq.length

// Part 3 - third refactor
let part2 : string -> int = 
  Seq.fold (fun ((h :: _) as xs ,ys) x -> (ys, move h x :: xs)) ([(0,0)],[(0,0)])
  >> function (xs,ys) -> xs @ ys
  >> Seq.distinct 
  >> Seq.length

input |> part1 |> printfn "Day 3 part 1: %d"
input |> part2 |> printfn "Day 3 part 2: %d"

// Part 2 - secondary refactor
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

// Part 2 - initial
let part2' dirs = 
    dirs 
    |> Seq.fold (fun (p1, p2, (s : (int * int) Set)) d -> 
                    let p' = move p1 d
                    (p2, p', (s.Add p'))) ((0,0), (0,0), Set.ofList [(0,0)])
    |> fun (_, _, s) -> s |> Seq.length

// Part 1 - initial
let countHouses dirs = 
    dirs 
    |> Seq.fold (fun (p,(s : (int * int) Set)) d -> 
                    let p' = move p d
                    (p', (s.Add p'))) ((0,0), Set.ofList [(0,0)])
    |> fun (_, s) -> s |> Seq.length