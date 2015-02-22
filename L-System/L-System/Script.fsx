type Symbol = | Sym of char
 
type State = Symbol list
 
type Rules = Map<Symbol,State>
 
type LSystem = 
    { Axiom:State
      Rules:Rules }
      
(*Pythagoras Tree encoding*)
 
let lSystem =
    { Axiom = [ Sym('0') ]
      Rules = [ Sym('1'), [ Sym('1'); Sym('1') ]
                Sym('0'), [ Sym('1'); Sym('['); Sym('0'); Sym(']'); Sym('0') ]]
              |> Map.ofList }
              
(*Growing from the original axiom
  by applying the rules*)
 
let applyRules (rs:Rules) (s:Symbol) =
    match (rs.TryFind s) with
    | None -> [s]
    | Some(x) -> x
 
let evolve (rs:Rules) (s:State) =
    [ for sym in s do yield! (applyRules rs sym) ]
 
let forward (g:LSystem) =
    let init = g.Axiom
    let gen = evolve g.Rules
    init |> Seq.unfold (fun state -> Some(state, gen state))
 
// compute nth generation of lSystem
let generation gen lSystem =
    lSystem
    |> forward 
    |> Seq.nth gen
    |> Seq.toList
    
(*Modelling the Turtle/Logo instructions*)
 
type Length = | Len of float
type Angle = | Deg of float
 
let add (a1:Angle) (a2:Angle) =
    let d1 = match a1 with Deg(x) -> x
    let d2 = match a2 with Deg(x) -> x
    Deg(d1+d2)
 
type Inst =
    | Move of Length
    | Turn of Angle
    | Push
    | Pop
 
let Fwd x = Move(Len(x))
let Lft x = Turn(Deg(x))
let Rgt x = Turn(Deg(-x))
 
(*
Transforming the L-system state
into a sequence of lines to draw
*)
 
type Pos = { X:float; Y:float; }
type Dir = { L:Length; A:Angle }
 
type Turtle = { Pos:Pos; Dir:Dir }
type ProgState = { Curr:Turtle; Stack:Turtle list }
 
let turn angle turtle = 
    let a = turtle.Dir.A |> add angle
    { turtle with Dir = { turtle.Dir with A = a } }
 
type Translation = Map<Symbol,Inst list>
 
type Ops = | Draw of Pos * Pos
 
let pi = System.Math.PI
 
let line (pos:Pos) (len:Length) (ang:Angle) =
    let l = match len with | Len(l) -> l
    let a = match ang with | Deg(a) -> (a * pi / 180.)
    { X = pos.X + l * cos a ; Y = pos.Y + l * sin a }
 
let execute (inst:Inst) (state:ProgState) =
    match inst with
    | Push -> None, { state with Stack = state.Curr :: state.Stack }
    | Pop -> 
        let head::tail = state.Stack // assumes more Push than Pop
        None, { state with Curr = head; Stack = tail }
    | Turn(angle) -> 
        None, { state with Curr =  state.Curr |> turn angle }
    | Move(len) -> 
        let startPoint = state.Curr.Pos
        let endPoint = line startPoint len state.Curr.Dir.A
        Some(Draw(startPoint,endPoint)), { state with Curr = { state.Curr with Pos = endPoint } }
 
let toTurtle (T:Translation) (xs:Symbol list) =
 
    let startPos = { X = 400.; Y = 400. }
    let startDir = { L = Len(0.); A = Deg(0.) }
    let init = 
        { Curr = { Pos = startPos; Dir = startDir }
          Stack = [] }
    xs 
    |> List.map (fun sym -> T.[sym]) 
    |> List.concat
    |> Seq.scan (fun (op,state) inst -> execute inst state) (None,init)
    |> Seq.map fst
    |> Seq.choose id
    
(*
Rendering using SVG
*)    
 
let header = """
<!DOCTYPE html>
<html>
<body>
<svg height="800" width="800">"""
 
let footer = """
</svg>
</body>
</html>
"""
 
let toSvg (ops:Ops seq) =
    let asString (op:Ops) = 
        match op with
        | Draw(p1,p2) -> sprintf """<line x1="%f" y1="%f" x2="%f" y2="%f" style="stroke:rgb(0,0,0);stroke-width:1" />""" p1.X p1.Y p2.X p2.Y 
 
    [ yield header
      for op in ops -> asString op
      yield footer ]
    |> String.concat "\n"
 
open System.IO
 
// fix the path for your own machine!
let path = @"c:\Temp\lsystem.html"
let save template = File.WriteAllText(path,template)
 
(*
Example: Sierpinski Triangle
http://en.wikipedia.org/wiki/L-system#Example_5:_Sierpinski_triangle
*)
 
let sierpinski () =
 
    let lSystem =
        { Axiom = [ Sym('A') ]
          Rules = [ Sym('A'), [ Sym('B'); Sym('>'); Sym('A'); Sym('>'); Sym('B') ]
                    Sym('B'), [ Sym('A'); Sym('<'); Sym('B'); Sym('<'); Sym('A') ]]
                  |> Map.ofList }
 
    let l = 1.
    let T = 
        [ Sym('A'), [ Fwd l; ]
          Sym('B'), [ Fwd l; ]
          Sym('>'), [ Lft 60.; ]
          Sym('<'), [ Rgt 60.; ] ]
        |> Map.ofList
 
    lSystem 
    |> generation 9
    |> toTurtle T
    |> toSvg 
    |> save
