let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input6.txt") |> Seq.filter (System.String.IsNullOrEmpty >> not)
let section (x1,y1) (x2,y2) =
    seq { for x in x1 .. x2 do 
            for y in y1 .. y2 do 
                yield (x,y) }
type Action = 
    | On
    | Off
    | Toggle

let parseAction (line : string) =
    let pp (p :string) =
        match p.Split([|','|]) with
        | [| x; y |] -> (System.Int32.Parse(x), System.Int32.Parse(y))
 
    match line.Split([|' '|]) with 
    | [| "toggle"; sg; _; eg |]  -> Toggle, (pp sg, pp eg)
    | [| _; "on"; sg; _; eg |]   -> On, (pp sg, pp eg)
    | [| _; "off"; sg; _; eg |]  -> Off, (pp sg, pp eg)
    | _ -> failwith "womp womp"

let part1 input = 
  let ls : int [,] = Array2D.zeroCreate 1000 1000
  let isOn (x,y) = ls.[x,y] = 1
  let turnOn (x,y) = ls.[x,y] <- 1
  let turnOff (x,y) = ls.[x,y] <- 0
  let toggle x = match x |> isOn with
                 | true -> turnOff x
                 | _    -> turnOn x
  input |> Seq.map parseAction 
  |> Seq.iter (fun (act, (x,y)) -> let f = act |> function On -> turnOn | Off -> turnOff | _ -> toggle
                                   section x y |> Seq.iter f)
  section (0,0) (999,999) |> Seq.filter isOn |> Seq.length

let part2 input = 
  let ls : int [,] = Array2D.zeroCreate 1000 1000
  let inc z (x,y) = ls.[x,y] <- ls.[x,y] + z
  let dec (x,y) = match ls.[x,y] with
                  | v when v > 0 -> ls.[x,y] <- v - 1
                  | _ -> ()
  input |> Seq.map parseAction 
  |> Seq.iter (fun (act, (x,y)) -> let f = act |> function On -> inc 1 | Off -> dec | _ -> inc 2
                                   section x y |> Seq.iter f)
  section (0,0) (999,999) |> Seq.map (fun (x,y) -> ls.[x,y]) |> Seq.sum
                 

input |> part1 |> printfn "Day 6 part 1: %d"
input |> part2 |> printfn "Day 6 part 2: %d"