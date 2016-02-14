type Ingredient = { Name : string
                    Capacity : int
                    Durability : int
                    Flavor : int
                    Texture : int
                    Calories : int }

let CI n c d f t cals = 
    { Name = n; Capacity = c; Durability = d; Flavor = f; Texture = t; Calories = cals }

let mult n i = 
    { i with Capacity = i.Capacity * n 
             Durability = i.Durability * n
             Flavor = i.Flavor * n
             Texture = i.Texture * n
             Calories = i.Calories * n }

let zeroMult a = function
    | b when b > 0 -> a * b
    | _            -> 0

let total ings = 
    ings |> Seq.fold (fun (c, d, f, t) i -> 
                            (c + i.Capacity, d + i.Durability, f + i.Flavor, t + i.Texture)) (0,0,0,0)
    |> fun (a,b,c,d) -> [a; b; c; d]
    |> Seq.fold (fun acc n -> zeroMult acc n) 1

let total' ings = 
    ings |> Seq.fold (fun (c, d, f, t, cal) i -> 
                            (c + i.Capacity, d + i.Durability, f + i.Flavor, t + i.Texture, cal + i.Calories)) (0,0,0,0,0)
    |> fun (a,b,c,d,cal) -> ([a; b; c; d] |> Seq.fold (fun acc n -> zeroMult acc n) 1), cal

let ingredients = [
    CI "Frosting"      4 -2  0 0 5
    CI "Candy"         0  5 -1 0 8
    CI "Butterscotch" -1  0  5 0 6
    CI "Sugar"         0  0 -2 2 1 ]

let combine ingredients = 
     let rec loop ingredients (accN, acc) = 
         seq { 
             match ingredients with 
             | [] -> yield acc 
             | [hd] -> 
                 yield (hd, 100-accN)::acc 
             | hd::tl -> 
                 for n in 0..100-accN do 
                 yield! loop tl (accN+n, (hd, n)::acc) 
         } 

     loop ingredients (0, []) 

let bake (ingredients : (Ingredient * int) seq) = 
    ingredients |> Seq.map (fun (i,n) -> mult n i)
    |> total

let bake' (ingredients : (Ingredient * int) seq) = 
    ingredients |> Seq.map (fun (i,n) -> mult n i)
    |> total'
    

let combinations = combine ingredients
let part1 = combinations |> Seq.map bake
            |> Seq.max
let part2 = combinations |> Seq.map bake' |> Seq.filter (fun (_,cal) -> cal = 500)
            |> Seq.max
