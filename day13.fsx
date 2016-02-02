open System
let file = __SOURCE_DIRECTORY__ + @"\input13.txt"


let sample = """
Alice would gain 54 happiness units by sitting next to Bob.
Alice would lose 79 happiness units by sitting next to Carol.
Alice would lose 2 happiness units by sitting next to David.
Bob would gain 83 happiness units by sitting next to Alice.
Bob would lose 7 happiness units by sitting next to Carol.
Bob would lose 63 happiness units by sitting next to David.
Carol would lose 62 happiness units by sitting next to Alice.
Carol would gain 60 happiness units by sitting next to Bob.
Carol would gain 55 happiness units by sitting next to David.
David would gain 46 happiness units by sitting next to Alice.
David would lose 7 happiness units by sitting next to Bob.
David would gain 41 happiness units by sitting next to Carol."""

type Person = { Name : string; NextTo : string; Happiness : int }
type Seating = { P1 : string; P2 : string; Total : int }
let parse (line : string) =
    let hap v = function
        | "gain" -> v
        | "lose" -> -v
         
    let a = line.Split([|' '|])
    { Name = a.[0] 
      NextTo = a.[10].Replace(".","")
      Happiness = a.[2] |> hap (Int32.Parse(a.[3])) }

let people input = 
    input
    |> Array.filter (fun l -> not <| String.IsNullOrEmpty l)
    |> Array.map parse
    |> Array.sortBy (fun {Happiness = h} -> h)
    |> Array.rev

let samplePeople = people (sample.Split([|'\n'|]))

let createPairings people = 
    [ for p in people do 
        let p' = people 
                    |> Seq.find (fun { Name = n; NextTo = n' } -> n = p.NextTo && n' = p.Name)
        yield if p.Name > p'.Name then {P1 = p'.Name; P2 = p.Name; Total = p.Happiness + p'.Happiness} 
                else {P1 = p.Name; P2 = p'.Name; Total = p.Happiness + p'.Happiness}]
    |> List.distinct
    |> List.sortBy (fun {Total = t} -> t)
    |> List.rev

let seatingArrangement pairings =
    let avail' n ps = ps |> List.filter (fun {P1 = p1; P2 = p2} -> p1 = n || p2 = n) 
                        |> List.length < 2
    let available ps p = ps |> avail' p.P1 && ps |> avail' p.P2
    let rec makeTable acc = function
        | []      -> acc
        | p :: tl -> 
            if available acc p then makeTable (p :: acc) tl
            else makeTable acc tl
    makeTable [] pairings 
        
let sumHappiness = List.map (fun {Total = t} -> t) >> List.sum

let part1 = file |> IO.File.ReadAllLines |> people |> createPairings |> seatingArrangement |> sumHappiness
let part2 = part1 -
                (file |> IO.File.ReadAllLines 
                |> people 
                |> createPairings 
                |> seatingArrangement
                |> (fun (h :: _) -> h.Total))