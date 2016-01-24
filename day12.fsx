open System
let file = __SOURCE_DIRECTORY__ + @"\input12.txt"

let sum = IO.File.ReadAllText(file).Split ([|'['; ']'; ','; '{';'}'; ':'|])
          |> Seq.filter (fun s -> not (s.StartsWith("\"") || s.Trim() |> String.IsNullOrEmpty))
          |> Seq.map Int32.Parse
          |> Seq.sum