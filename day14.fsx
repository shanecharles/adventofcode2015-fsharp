open System
type Reindeer = { Name : string; Speed : int; Duration : int; Rest : int }
let parseLine (line : string) = let a = line.Split([|' '|])
                                { Name = a.[0]
                                  Speed = a.[3] |> Int32.Parse 
                                  Duration = a.[6] |> Int32.Parse
                                  Rest = a.[13] |> Int32.Parse }

let deer = IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "/input14.txt")
           |> Seq.filter (String.IsNullOrEmpty >> not)
           |> Seq.map parseLine

let raceTime = 2503
let race time r = let fullRun = r.Duration * r.Speed
                  let interval = r.Duration + r.Rest
                  let cycle = time / interval
                  let rem = time % interval
                  r.Name, cycle * fullRun + 
                          if rem >= r.Duration then fullRun
                          else rem * r.Speed 

let part1 : (Reindeer seq) -> int = Seq.map (race raceTime) >> Seq.maxBy snd >> snd

let part2 xs = let racers = List.ofSeq xs
               let dict = new System.Collections.Generic.Dictionary<string,int>()
               racers |> List.iter (fun {Name=n} -> dict.Add(n, 0))
               let awardPoint name = dict.[name] <- 1 + dict.[name]
               let calcPoints x = racers |> Seq.map (race x) |> Seq.groupBy snd 
                                  |> Seq.maxBy fst |> snd |> Seq.iter (fst >> awardPoint)
               {1 .. raceTime} |> Seq.iter calcPoints
               dict.Values |> Seq.max

deer |> part1 |> printfn "Day 14 part 1 = %d"
deer |> part2 |> printfn "Day 14 part 2 = %d"