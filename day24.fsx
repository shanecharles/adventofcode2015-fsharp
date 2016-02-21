open System

let file = __SOURCE_DIRECTORY__ + "/input24.txt"
let presents = file |> IO.File.ReadAllLines
               |> Array.filter (String.IsNullOrEmpty >> not)
               |> Array.map Int32.Parse
               |> Array.rev

let balance3 = presents |> Array.sum |> fun x -> x / 3
let balance4 = presents |> Array.sum |> fun x -> x / 4
let hi,lo = presents |> Array.partition (fun x -> x > 100)

let combos = 
  [ for i in 0 .. (hi.Length - 1) do 
      yield hi |> Array.mapi (fun idx x -> if idx = i then -1 else x)
            |> Array.filter (fun x -> x >= 0)
            |> Array.toList ]

let minLength balance bin ps = 
  let rec makeBin acc a =
    match acc |> Seq.sum with 
    | x when x >= balance -> acc
    | x -> match a with
           | []       -> acc
           | hd :: tl -> 
             if x + hd <= balance then 
               makeBin (hd :: acc) tl
             else
               makeBin acc tl
  makeBin (bin |> List.rev) (ps |> Array.toList)

let lows = combos |> List.map (fun l -> minLength balance3 l lo)
let minPresents = lows |> List.map List.length |> List.min
let minBins = lows |> List.filter (fun l -> l |> List.length = minPresents)
              |> List.map List.rev
              |> List.map (List.take 4)

let makeCombosOf amount = 
  seq { for i in { 0 .. (lo.Length - 2)} do 
          for j in { i + 1 .. (lo.Length - 1)} do 
            if lo.[i] + lo.[j] = amount then yield [lo.[i]; lo.[j]] }

let minProduct = makeCombosOf >> Seq.map (Seq.reduce (*)) >> Seq.min

let part1 = minBins |> List.map (fun l -> (l |> List.reduce (*), l |> List.sum))
            |> List.map (fun (p,x) -> p, balance3 - x |> minProduct)
            |> List.map (fun (p,x) -> int64 p * int64 x)
            |> List.min

let part2 = minLength balance4 [] presents |> List.map int64 |> List.reduce (*)