open System
let file = __SOURCE_DIRECTORY__ + @"/input23.txt"

type Instruction =
  | HLF of string
  | TPL of string
  | INC of string
  | JMP of int
  | JIE of string * int
  | JIO of string * int

let createInstruction (l : string) = 
  let ci (a, b) = function
    | "hlf" -> HLF a
    | "tpl" -> TPL a
    | "inc" -> INC a
    | "jmp" -> JMP (Int32.Parse a)
    | "jie" -> JIE (a, Int32.Parse b)
    | "jio" -> JIO (a, Int32.Parse b)
  
  l.Split([|' '; ','|], StringSplitOptions.RemoveEmptyEntries) 
  |> function
  | [| ins; a |]    -> ci (a, "") ins
  | [| ins; a; b |] -> ci (a, b) ins

type Registers = { a : int; b : int }
let initRegisters = { a = 0; b = 0 }

let applyReg (regs : Registers) fn = function
  | "a" -> { regs with a = regs.a |> fn }
  | _   -> { regs with b = regs.b |> fn }

let multr regs d = applyReg regs ((*) d) 
let halfr regs = applyReg regs (fun x -> x / 2) 
let incr regs = applyReg regs ((+) 1)
let read regs = function
  | "a" -> regs.a
  | _   -> regs.b

type JumpOneOrEven =
  | Even
  | One
  | NoJump

let oddEvenReg regs = 
  read regs >> (fun x -> x, x % 2) 
  >> function
  | 1,_ -> One
  | _,0 -> Even
  | _ -> NoJump

let rec execute (ins : Instruction []) (i, regs) = 
  let inc = ((+) 1)
  if i >= ins.Length then regs
  else
    let instruction = ins.[i]
    let (i', regs') = 
      match instruction with
      | HLF r     -> (inc i, halfr regs r)
      | TPL r     -> (inc i, multr regs 3 r)
      | INC r     -> (inc i, incr regs r)
      | JMP o     -> (i + o, regs)
      | JIE (r,o) -> 
        match oddEvenReg regs r with
        | Even -> (i + o, regs)
        | _    -> (inc i, regs)
      | JIO (r,o) -> 
        match oddEvenReg regs r with
        | One -> (i + o, regs)
        | _   -> (inc i, regs)
    execute ins (i',regs')

let instructions = file |> IO.File.ReadAllLines
                   |> Array.map createInstruction
                   
let part1 = execute instructions (0,initRegisters)