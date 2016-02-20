type Stats = { Hp : int; Attack : int; Defense : int }
type StoreItem = 
  { Cost : int; Damage : int; Armor : int }
  static member (+) (a : StoreItem, b : StoreItem) = 
    { Cost = a.Cost + b.Cost; Damage = a.Damage + b.Damage; Armor = a.Armor + b.Armor }

let battlePower = List.map (fun { StoreItem.Damage = d; Armor = a} -> d + a) >> List.sum

let boss = { Hp = 104; Attack = 8; Defense = 1}

let csi c d a = { Cost = c; Damage = d; Armor = a }

let weapons = [ csi 8 4 0
                csi 10 5 0
                csi 25 6 0
                csi 40 7 0
                csi 74 8 0 ]

let armor = [ csi 13 0 1
              csi 31 0 2
              csi 53 0 3
              csi 75 0 4
              csi 102 0 5 ]

let rings = [ csi 25 1 0
              csi 50 2 0
              csi 100 3 0
              csi 20 0 1
              csi 40 0 2
              csi 80 0 3 ]

let createCombos items =
  let rec rc acc (rs : StoreItem list) =
    match rs with 
    | [] -> []
    | r :: tl -> (tl |> List.map (fun r' -> r + r')) @ rc acc tl
  rc [] items

let ringCombos = (createCombos rings) @ rings |> List.sortBy (fun {Cost = c} -> c)

let purchases (ws : StoreItem list) arms rs = 
  seq { for w in ws do 
          yield w
          for a in arms do
            yield w + a
            for r in rs do 
              yield w + r
              yield w + a + r }

type Winner = 
  | Player
  | Boss
  | NoWinner

let simulate b p = 
  let getAttack a d = 
    match a - d with
    | x when x < 1 -> 1
    | x            -> x
  let pAttack = getAttack p.Attack b.Defense
  let bAttack = getAttack b.Attack p.Defense

  let determineWinner x =
    match (x*bAttack, x*pAttack) with
    | _, bh when bh >= b.Hp -> Some Player
    | ph, _ when ph >= p.Hp -> Some Boss
    | _, _                  -> None

  let hitsToWin = min (b.Hp / pAttack) (p.Hp / bAttack)
  { hitsToWin .. (hitsToWin + 1)} |> Seq.choose determineWinner
  |> Seq.head

let simulateBattle b s = 
  let p = { Hp = 100; Attack = s.Damage; Defense = s.Armor }
  (simulate b p, s)
  
let part1 = purchases weapons armor ringCombos
            |> Seq.map (simulateBattle boss)
            |> Seq.choose (fun (w,s) -> match w with 
                                        | Player -> Some s
                                        | _   -> None)
            |> Seq.sortBy (fun {Cost = c} -> c)
            |> Seq.head

let part2 = purchases weapons armor ringCombos
            |> Seq.map (simulateBattle boss)
            |> Seq.choose (fun (w,s) -> match w with 
                                        | Boss -> Some s
                                        | _   -> None)
            |> Seq.sortByDescending (fun {Cost = c} -> c)
            |> Seq.head