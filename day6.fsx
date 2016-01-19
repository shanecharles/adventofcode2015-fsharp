let section (x1,y1) (x2,y2) =
    seq { for x in x1 .. x2 do 
            for y in y1 .. y2 do 
                yield (x,y) }

let isOn (s : (int * int) Set) = s.Contains
let turnOn (s : (int * int) Set) light = s.Add(light)
let turnOff (s : (int * int) Set) light = s.Remove light
let toggleLight s light =
    match light |> isOn s with
    | true -> turnOff s light
    | _    -> turnOn s light

let turnLightsOn s = Seq.fold turnOn s
let turnLightsOff s = Seq.fold turnOff s
let toggleLights s = Seq.fold toggleLight s

let sampleOn = "turn on 489,959 through 759,964"
let sampleOff = "turn off 820,516 through 871,914"
let sampleToggle = "toggle 756,965 through 812,992"

type Action = 
    | On
    | Off
    | Toggle

let getAction = function
    | On  -> turnLightsOn
    | Off -> turnLightsOff
    | _   -> toggleLights

let parseAction (line : string) =
    let pp (p :string) =
        match p.Split([|','|]) with
        | [| x; y |] -> (System.Int32.Parse(x), System.Int32.Parse(y))
 
    match line.Split([|' '|]) with 
    | [| "toggle"; sg; _; eg |]  -> (pp sg, pp eg), Toggle
    | [| _; "on"; sg; _; eg |]   -> (pp sg, pp eg), On
    | [| _; "off"; sg; _; eg |]  -> (pp sg, pp eg), Off
    | _ -> failwith "womp womp"

let grid, act = parseAction sampleOn

let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\input6.txt")

let actions = input |> Array.map parseAction

// Part 2
let increase amt (m : Map<(int * int), int>) light = 
    let b = match m.TryFind light with
            | Some b -> b + amt
            | None   -> amt
    m.Add(light, b)

let turnLightsOn' m ls = ls |> Seq.fold (increase 1) m
let toggleLights' m ls = ls |> Seq.fold (increase 2) m
let turnLightsOff' (m : Map<(int * int), int>) lights =
    let turnOff (m' : Map<(int * int), int>) light = 
        match m'.TryFind light with
        | Some b when b > 1 -> m'.Add (light, b - 1)
        | Some b            -> m'.Remove light
        | _                 -> m'

    lights |> Seq.fold (fun m'' l -> turnOff m'' l) m

let getAction' = function
    | On  -> turnLightsOn'
    | Off -> turnLightsOff'
    | _   -> toggleLights'

let totalLightsOn = actions |> Array.fold (fun lights (g, a) ->
                        g ||> section |> (getAction a) lights) (Set.empty)
                    |> Set.count

let totalBrightness = 
    actions |> Array.fold (fun lights (g, a) ->
                                    g ||> section |> (getAction' a) lights) (Map.empty)
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.sum 