let input = "yzbqklnj"

let md5 = System.Security.Cryptography.MD5.Create ()
let computeHash (md5 : System.Security.Cryptography.MD5) (s : string) =
    md5.ComputeHash(s |> Seq.map (fun c -> (byte) c) |> Seq.toArray)
    |> Array.map (fun b -> b.ToString("X2"))
    |> function xs -> System.String.Join ("", xs)

let ch = computeHash md5

let adventCoin prefix input = 
    Seq.initInfinite (fun i -> 
        let r = i |> sprintf "%s%d" input |> ch
        if r.StartsWith(prefix) then Some i
        else None)
    |> Seq.choose id |> Seq.head

input |> adventCoin "00000" |> printfn "Day 4 part 1: %d"
input |> adventCoin "000000" |> printfn "Day 4 part 2: %d"