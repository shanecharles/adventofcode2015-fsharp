open System
open System.Collections.Generic
open System.Text.RegularExpressions

let file = __SOURCE_DIRECTORY__ + "/input7.txt"
let outputs = file |> IO.File.ReadAllLines |> Array.filter (String.IsNullOrEmpty >> not)
              |> Array.map (fun l -> 
                              let a = l.Split([| "->" |], StringSplitOptions.None)
                              (a.[1].Trim(), a.[0].Trim()))

let (|RegexMatch|_|) pattern input = 
//let rmatch pattern input = 
  let m = Regex.Match(input, pattern)
  if m.Success
  then [ for g in m.Groups -> g.Value ] |> List.tail |> Some
  else None

let WireSignal circuit w =
  let d = new Dictionary<string,uint16>()
  let rec getSignal (cache : Dictionary<string,uint16>)  (src : string -> string) wire = 
    let add w n = cache.Add(w,n) |> ignore; 
                  cache.[w]
    if cache.ContainsKey wire then cache.[wire]
    else
      match wire |> src with 
      | RegexMatch @"NOT (\w+)" [c] -> UInt16.MaxValue - (getSignal cache src c)
      | RegexMatch @"(\w+) RSHIFT ([0-9]+)" [c; n] ->
        (getSignal cache src c) >>> (Int32.Parse n)
      | RegexMatch @"(\w+) LSHIFT ([0-9]+)" [c; n] ->
        (getSignal cache src c) <<< (Int32.Parse n)
      | RegexMatch @"([0-9]+) AND (\w+)" [n; a] ->
        (UInt16.Parse n) &&& (getSignal cache src a)
      | RegexMatch @"([0-9]+) OR (\w+)" [n; a] ->
        (UInt16.Parse n) ||| (getSignal cache src a)
      | RegexMatch @"(\w+) AND (\w+)" [a; b] ->
        (getSignal cache src a) &&& (getSignal cache src b)
      | RegexMatch @"(\w+) OR (\w+)" [a; b] ->
        (getSignal cache src a) ||| (getSignal cache src b)
      | RegexMatch @"([0-9]+)" [n]  -> UInt16.Parse n
      | RegexMatch @"(\w+)" [c]     -> getSignal cache src c 
      |> add wire
  getSignal d circuit w
        
let findWire (circuits : (string*string) []) wire =
  circuits |> Array.find (fun (w,_) -> w = wire) |> snd

let part1 = WireSignal (findWire outputs) "a"
// Reqs
// - Signals provided by gate, another wire, or a specific value
// - Wire can only receive from one source
// - Wire can provide a signal to multiple destinations
// - gates only provide output when all inputs have a signal