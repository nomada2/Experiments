open System
//    
type Item =
    | Int
    | Dot
    | Empty
    | Pipe 
    | Row of Item list
    | Col of Item list
   

let problem = 
 """.  .  4 | 8  .  . | .  1  7    
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
    2  4  . | .  .  1 | 5  .  ."""

let solution =
 """9  3  4 | 8  2  5 | 6  1  7    
            |         |
    6  7  2 | 9  1  4 | 8  5  3
            |         |
    5  1  8 | 6  3  7 | 9  2  4
    --------+---------+--------
    3  2  5 | 7  4  8 | 1  6  9
            |         |
    4  6  9 | 1  5  3 | 7  8  2
            |         |
    7  8  1 | 2  6  9 | 4  3  5
    --------+---------+--------
    1  9  7 | 5  8  2 | 3  4  6
            |         |
    8  5  3 | 4  7  6 | 2  9  1
            |         |
    2  4  6 | 3  9  1 | 5  7  8"""

// Every spot in the puzzle belongs to a (horizontal) row and a (vertical) column, as well as to one single 3x3
// square (which we call "square" for short). At the beginning, some of the spots carry a single-digit number between 1 and 9. 
// The problem is to fill the missing spots with digits in such a way that every number between 1 and 9 appears
// exactly once in each row, in each column, and in each square.

let isValidTable (i, j) (i', j') =
    i=i' || j=j' || (i-1)/3=(i'-1)/3 && (j-1)/3=(j'-1)/3


let selectPoint point value point' values =
    if point = point' then [value] else
      if isValidTable point point' then values |> List.filter (fun x -> x <> value) else values

let increment coordinate value sols =
    List.map (fun (value', values) -> value', selectPoint coordinate value value' values) sols

let rec bruteFrce f sol = function
    | [] -> f sol
    | (point, values)::sols ->
       
        let compareValues value =
                let  (_, ns1), (_, ns2) = value
                let len = List.length ns1
                let len' = List.length ns2
                compare len len'

        let sols = List.sortWith(fun  (_, ns1) (_, ns2) -> compare (List.length ns1) (List.length ns2)) sols
        List.iter (fun n -> bruteFrce f (Map.add point n sol) (increment point n sols)) values

let size = 3

let size2 = size * size


let table =
        let cs =
             seq { for line in problem.Split("\n".ToCharArray()) do
                        for char in line do
                            if Char.IsDigit(char) then yield char
                            elif char = '.' then yield '0'
                        yield '\n' } |> Seq.toArray// |> string
        new String(cs)

        

let display x = 
    x |> Array.iter (fun b -> 
                    Array.iter (fun x -> printf "%d " x) b; 
                    printfn "";
                 )
                 

let verify1 (grid:int[][]) x y c = 
    [0 .. 8] |> List.forall (fun i -> grid.[i].[y] <> c && grid.[x].[i]<> c)

let verify2 (grid:int[][]) x y c = 
          let xs = (x/3)*3 in
          let ys = (y/3)*3 in   
            [xs .. (xs+2)] |> List.forall (fun i -> 
                [ys .. (ys+2)] |> List.forall (fun j -> (grid.[i].[j] <> c)))

let verify grid x y c = 
    if (verify1 grid x y c) then (verify2 grid x y c) else false




let rec solve x y (grid:int[][]) =
   if grid.[x].[y] <> 0 then
      if (x = 8) && ( y = 8) then
         display grid
      else
         solve ((x+1) % 9) (if x = 8 then (y+1) else y) grid
   else
         [1..9] |> List.iter  (fun c ->
            if (verify grid x y c) then
               Array.set grid.[x] y c;
               solve x y grid;
            else
               ()
            Array.set grid.[x] y 0;
          )               
          
//
//let table =  
//    let lines = problem.Split("\n".ToCharArray())
//    let item (c:char) = 
//                 match c with
//                 | "." -> None
//                 | c when Char.IsDigit(c) -> Some(int(c))
//
//    let table = // (x,y,value)
//        lines 
//            |> Array.mapi(fun index c -> 
//    seq {
//            for line in lines do
//                for c in line do 
//                    if Char.IsDigit(c) then yield Some( int(c))
//                    elif Char.IsPunctuation(c) then yield None }



