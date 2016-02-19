let input = 33100000

let rec Elves h = 
  seq { let s = h |> float |> sqrt |> int
        for i in { s .. -1 .. 1 } do 
                   if h % i = 0 then
                     if h / i <> i then
                       yield h / i
                       yield i }

let presents h = h |> Elves |> Seq.sum |> (fun s -> (h,s * 10))
let presents' h = h |> Elves |> Seq.filter (fun s -> h / s < 51) |> Seq.sum |> (fun s -> (h,s * 11))

let findFirstHouse pres inp = 
  Seq.map pres >> Seq.filter (fun (h,p) -> p >= input)
  >> Seq.head

let part1 = {1 .. System.Int32.MaxValue} |> findFirstHouse presents input
let part2 = {1 .. System.Int32.MaxValue} |> findFirstHouse presents' input
