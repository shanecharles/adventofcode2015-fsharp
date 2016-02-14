open System

let input = __SOURCE_DIRECTORY__ + "/input18.txt"
let lights = input |> IO.File.ReadAllLines
             |> Array.mapi (fun i l -> 
                l |> Seq.mapi (fun j c -> 
                    match c with 
                    | '#' -> Some (i,j)
                    | _ -> None)
                |> Seq.choose id
                |> Seq.toArray)
             |> Array.concat
             |> List.ofArray 

let neighbors (x,y) = 
  let inGrid = function x when x >= 0 && x <= 99 -> true | _ -> false
  [ for i in -1 .. 1 do 
    for j in -1 .. 1 do 
      let x' = x + i
      let y' = y + j
      if x' |> inGrid && y' |> inGrid && (i <> 0 || j <> 0) 
      then yield x', y' ]

let evolve ls =
  ls |> List.map neighbors
  |> List.concat
  |> List.groupBy id
  |> List.filter (fun (l,ns) -> ns |> Seq.length |> function 
                                                    | 2 when ls |> List.exists (fun l' -> l = l') -> true
                                                    | 3 -> true 
                                                    | _ -> false)
  |> List.map fst

let part1 = [1 .. 100] |> List.fold (fun ls _ -> evolve ls) lights |> List.length
let addCorners ls = (0,0) :: (99,0) :: (0,99) :: (99,99) :: ls |> List.distinct 
let part2 = [1 .. 100] |> List.fold (fun ls _ -> ls |> evolve |> addCorners) lights |> List.length