let initial = 20151125L
let location = (2947, 3029)
let nextCode (pcode : int64) = pcode * 252533L % 33554393L

let createGrid (x,y) =
  seq { let mutable found = false
        for i in 1 .. (2 * max x y) do 
        for r in i .. -1 .. 1 do 
          if not found then
            let r,c = (r,i + 1 - r)
            yield r,c
            found <- r = x && c = y } 

let calcCode init loc = 
  loc |> createGrid |> Seq.skip 1
  |> Seq.fold (fun code _ -> nextCode code) init

let part1 = calcCode initial location