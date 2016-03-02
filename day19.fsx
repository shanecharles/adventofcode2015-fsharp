open System
open System.Text.RegularExpressions

let file = __SOURCE_DIRECTORY__ + "/input19.txt"
let split (l : string) = let ls = l.Split([|" => "|], StringSplitOptions.None) in ls.[0],ls.[1]
let group = Array.groupBy fst >> Array.map (fun (h,hs) -> h, hs |> Array.map snd |> Array.toList) >> Array.toList

let molecule,replacements = 
  file |> IO.File.ReadAllLines
  |> Array.filter (String.IsNullOrEmpty >> not)
  |> (fun a -> a.[a.Length-1],a |> Array.take (a.Length-1) |> Array.map split |> group |> List.sortBy (fun (a,_) -> a.Length))

let segments = molecule.Split([|"Ar"|], StringSplitOptions.None)

let s = segments |> Seq.skip 1 |> Seq.head

let calcReplacements (reps : (string * string list) list) (segment : string) =
  let rec replace acc used rem = 
    let (acc', used', rem') = 
      match rem with
      | a :: tl  when reps |> List.exists (fun (c,_) -> a.ToString() = c) ->
        let rs = reps |> List.choose (fun (c,rs) -> if a.ToString() = c then Some rs else None) |> Seq.head
        ((rs, used |> List.rev, tl) :: acc, (a :: used), tl)
      | a :: b :: tl when reps |> List.exists (fun (c,_) -> String.Join("",[|a.ToString();b.ToString()|]) = c) ->
        let rs = reps |> List.choose (fun (c,rs) -> if String.Join("",[|a.ToString();b.ToString()|]) = c then Some rs else None) |> Seq.head
        ((rs, used |> List.rev, tl) :: acc, (a :: used), (b :: tl))
      | a :: tl -> (acc, (a :: used), tl)
      | [] -> (acc, used, [])

    if rem' |> List.isEmpty then acc'
    else replace acc' used' rem'
  replace [] [] (segment |> Seq.toList)

let replaceCount reps = 
  let mergeReplacements ((rs : string list), hd, tl) = rs |> List.map (fun s -> hd @ (s |> Seq.toList) @ tl)
  reps |> Seq.map mergeReplacements
  |> Seq.concat
  |> Seq.distinct
  |> Seq.length

let crs = calcReplacements replacements
let par1 = segments |> Seq.map crs |> Seq.map replaceCount |> Seq.sum