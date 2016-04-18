open System
let area (x : int) y = x * y
let double = ((*) 2) >> area

let presentArea (l, w, h) = 
    double l w + double w h + double h l

let presentSlack (l, w, h) =
    [|(area l w); (area w h); (area h l)|] |> Array.min

let presentWrapping p = presentArea p + presentSlack p

let ribbon (l, w, h) = [l;w;h] |> List.sort
                       |> List.take 2
                       |> List.sum 
                       |> ((*) 2)

let bow (l, w, h) = l * w * h

let presentRibbon p = ribbon p + bow p


let test = "1x2x3"
let parseDimensions (p : string) = p.Split('x') |> Array.map Int32.Parse
                                   |> fun [|l; w; h|] -> (l, w, h)

let presents = IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"/input2.txt")
               |> Array.map parseDimensions

let totalWrapping = Array.map presentWrapping >> Array.sum
let totalRibbon = Array.map presentRibbon >> Array.sum
presents |> totalWrapping |> printfn "Day 2 part 1: %d"
presents |> totalRibbon |> printfn "Day 2 part 2: %d"
