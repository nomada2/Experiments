open System.Collections.Generic
open System

let problem =
  """ .  .  4 | 8  .  . | .  1  7  
              |         |          
      6  7  . | 9  .  . | .  .  .  
              |         |          
      5  .  8 | .  3  . | .  .  4  
      --------+---------+--------  
      3  .  . | 7  4  . | 1  .  .  
              |         |          
      .  6  9 | .  .  . | 7  8  .  
              |         |          
      .  .  1 | .  6  9 | .  .  5  
      --------+---------+--------  
      1  .  . | .  8  . | 3  .  6  
              |         |          
      .  .  . | .  .  6 | .  9  1  
              |         |          
      2  4  . | .  .  1 | 5  .  .  
  """
let problem' =
  [ for line in problem.Split('\n') do
      let parsed =
        [ for c in line do
            if c = '.' then yield 0
            elif Char.IsNumber(c) then yield (int c - 48) ] 
      if parsed <> [] then yield parsed ]
  

type Sudoku = int array array

let rows = id

let cols (sudoku:Sudoku) =
    sudoku
    |> Array.mapi (fun a row -> row |> Array.mapi (fun b cell -> sudoku.[b].[a]))

let getBoxIndex count row col = 
   let n = row/count
   let m = col/count
   n * count + m

let boxes (sudoku:Sudoku) = 
    let d = sudoku |> Array.length |> float |> System.Math.Sqrt |> int
    let list = new List<_>()
    for a in 0..(d*d) - 1 do list.Add(new List<_>())

    for a in 0..(Array.length sudoku - 1) do
        for b in 0..(Array.length sudoku - 1) do
            list.[getBoxIndex d a b].Add(sudoku.[a].[b])

    list 
      |> Seq.map Seq.toArray
  
let toSudoku s = 
    s |> Seq.map Seq.toArray
      |> Seq.toArray   

let allUnique numbers =
    let set = new HashSet<_>()
    numbers
    |> Seq.filter ((<>) 0)
    |> Seq.forall set.Add



let solvable sudoku =
    rows sudoku
    |> Seq.append (cols sudoku)
    |> Seq.append (boxes sudoku)
    |> Seq.forall allUnique

let replaceAtPos (x:Sudoku) row col newValue :Sudoku =     
    [| for a in 0..(Array.length x - 1) ->
        [| for b in 0..(Array.length x - 1) -> 
            if a = row && b = col then newValue else x.[a].[b] |] |]

let rec substitute row col (x:Sudoku) = 
    let a,b = if col >= Array.length x then row+1,0 else row,col
    if a >= Array.length x then seq { yield x } else
    if x.[a].[b] = 0 then 
        [1..Array.length x]           
            |> Seq.map (replaceAtPos x a b)  
            |> Seq.filter solvable                     
            |> Seq.map (substitute a (b+1))
            |> Seq.concat
     else substitute a (b+1) x

let getFirstSolution = substitute 0 0 >> Seq.head

let solve sudoku =
    let solution = getFirstSolution sudoku
    printfn "%A" solution

let s = problem' |> toSudoku
    
solve s
    
