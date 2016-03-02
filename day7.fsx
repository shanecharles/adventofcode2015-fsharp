open System
open System.Collections.Generic
open System.Text.RegularExpressions

let file = __SOURCE_DIRECTORY__ + "/input7.txt"
let outputs = file |> IO.File.ReadAllLines |> Array.filter (String.IsNullOrEmpty >> not)
              |> Array.map (fun l -> 
                              let a = l.Split([| "->" |], StringSplitOptions.None)
                              (a.[1].Trim(), a.[0].Trim()))

let (|RegexMatch|_|) pattern input = 
  let m = Regex.Match(input, pattern)
  if m.Success
  then [ for g in m.Groups -> g.Value ] |> List.tail |> Some
  else None

let WireSignal (circuit : string -> string) w =
  let cache = new Dictionary<string,uint16>()
  let rec getSignal wire = 
    let add w n = cache.Add(w,n) |> ignore; 
                  cache.[w]
    if cache.ContainsKey wire then cache.[wire]
    else
      match wire |> circuit with 
      | RegexMatch @"NOT (\w+)" [c] -> UInt16.MaxValue - (getSignal c)
      | RegexMatch @"(\w+) RSHIFT ([0-9]+)" [c; n] ->
        (getSignal c) >>> (Int32.Parse n)
      | RegexMatch @"(\w+) LSHIFT ([0-9]+)" [c; n] ->
        (getSignal c) <<< (Int32.Parse n)
      | RegexMatch @"([0-9]+) AND (\w+)" [n; a] ->
        (UInt16.Parse n) &&& (getSignal a)
      | RegexMatch @"([0-9]+) OR (\w+)" [n; a] ->
        (UInt16.Parse n) ||| (getSignal a)
      | RegexMatch @"(\w+) AND (\w+)" [a; b] ->
        (getSignal a) &&& (getSignal b)
      | RegexMatch @"(\w+) OR (\w+)" [a; b] ->
        (getSignal a) ||| (getSignal b)
      | RegexMatch @"([0-9]+)" [n]  -> UInt16.Parse n
      | RegexMatch @"(\w+)" [c]     -> getSignal c 
      |> add wire
  getSignal w
        
let findWire (circuits : (string*string) []) wire =
  circuits |> Array.find (fun (w,_) -> w = wire) |> snd

let part1 = WireSignal (findWire outputs) "a"

let outputs2 = outputs |> Array.map (function 
                                     | "b",_ -> "b",part1.ToString()
                                     | w,n   -> w,n)

let part2 = WireSignal (findWire outputs2) "a"