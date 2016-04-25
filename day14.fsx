open System

let data = """
Rudolph can fly 22 km/s for 8 seconds, but then must rest for 165 seconds.
Cupid can fly 8 km/s for 17 seconds, but then must rest for 114 seconds.
Prancer can fly 18 km/s for 6 seconds, but then must rest for 103 seconds.
Donner can fly 25 km/s for 6 seconds, but then must rest for 145 seconds.
Dasher can fly 11 km/s for 12 seconds, but then must rest for 125 seconds.
Comet can fly 21 km/s for 6 seconds, but then must rest for 121 seconds.
Blitzen can fly 18 km/s for 3 seconds, but then must rest for 50 seconds.
Vixen can fly 20 km/s for 4 seconds, but then must rest for 75 seconds.
Dancer can fly 7 km/s for 20 seconds, but then must rest for 119 seconds.
"""

type Reindeer = { Name : string
                  Speed : int 
                  Duration : int
                  Rest : int }

let parseLine (line : string) = 
    let a = line.Split([|' '|])
    { Name = a.[0]
      Speed = a.[3] |> Int32.Parse 
      Duration = a.[6] |> Int32.Parse
      Rest = a.[13] |> Int32.Parse }

let deer = data.Split([|'\n'|])
           |> Array.filter (String.IsNullOrEmpty >> not)
           |> Array.map parseLine

let raceTime = 2503

let race time r = 
    let fullRun = r.Duration * r.Speed
    let interval = r.Duration + r.Rest
    let cycle = time / interval
    let rem = time % interval
    r.Name, cycle * fullRun + 
            if rem >= r.Duration then fullRun
            else rem * r.Speed 

let part1 = deer |> Array.map (race raceTime)
            |> Array.sortByDescending snd
