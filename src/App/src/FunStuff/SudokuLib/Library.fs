
//type SudokuSolver() = 
    

module SudokuSolverModule 
    
    open System

    let setRow = [1..9] |> set
    let removeSets s = setRow - ( s |> set)

    let validateRow index emptyCellIndex =
        (index / 9 = emptyCellIndex / 9) || (index % 9 = emptyCellIndex % 9) || (((index % 9) / 3 + (index / 9) / 3 * 3) = ((emptyCellIndex % 9) / 3 + (emptyCellIndex / 9) / 3 * 3))
       
    
    let replaceAtIndex index newValue lst = 
       let mapi = lst |> List.mapi(fun i c -> (i,c))
       let rec replaceAtIndex index newValue lst acc = 
            match lst with
            | (i,c)::t when i = index -> ((i,newValue)::acc |> List.rev) @ t |> List.map snd
            | h::t -> replaceAtIndex index newValue t (h::acc)
            | _ -> lst  |> List.map snd
       replaceAtIndex index newValue mapi []

    let rec sudokuSolver (cells:int list) =
        let cellMapped = cells |> List.mapi(fun i c -> (c,i))
        let zeroCell = cellMapped |> List.tryFind(fun (c,i) -> c = 0)
        match zeroCell with
        | None -> cells
        | Some(_,z) ->  let setRow = removeSets (cellMapped 
                                                |> List.filter(fun (c, i) -> validateRow i z)
                                                |> List.map fst)
                        let rec findSolution (grid:int list) (lstRow:int list)  =
                            match lstRow with
                            | [] -> []
                            | h::t -> let grid' = replaceAtIndex z h grid
                                      match sudokuSolver grid' with
                                      | [] -> findSolution grid' t   
                                      | c -> c       
                        findSolution cells (setRow |> Set.toList)




                            
    let sudokuPuzzle = [
                   2; 0; 0; 0; 0; 0; 1; 9; 0;
                   0; 0; 1; 0; 8; 0; 0; 0; 5;
                   0; 0; 0; 5; 4; 0; 0; 2; 0;
                   0; 0; 7; 6; 0; 0; 0; 0; 0;
                   8; 5; 0; 0; 0; 0; 0; 7; 1;
                   0; 0; 0; 0; 0; 8; 5; 0; 0;
                   0; 4; 0; 0; 9; 7; 0; 0; 0;
                   9; 0; 0; 0; 6; 0; 7; 0; 0;
                   0; 6; 3; 0; 0; 0; 0; 0; 2 ]

    let sudokuPuzzle2 = [
         0;0;4;8;0;0;0;1;7;    
         6;7;0;9;0;0;0;0;0;    
         5;0;8;0;3;0;0;0;4;
         3;0;0;7;4;0;1;0;0;  
         0;6;9;0;0;0;7;8;0;
         0;0;1;0;6;9;0;0;5;
         1;0;0;0;8;0;3;0;6;
         0;0;0;0;0;6;0;9;1;
         2;4;0;0;0;1;5;0;0 ] //  |> List.mapi(fun i v -> (i,v))

    let solution  = [
        9;3;4;8;2;5;6;1;7;
        6;7;2;9;1;4;8;5;3;
        5;1;8;6;3;7;9;2;4;
        3;2;5;7;4;8;1;6;9;
        4;6;9;1;5;3;7;8;2;
        7;8;1;2;6;9;4;3;5;
        1;9;7;5;8;2;3;4;6;
        8;5;3;4;7;6;2;9;1;
        2;4;6;3;9;1;5;7;8]

    let printState (state:int list) =
        //let state' = state |> List.mapi(fun i v -> (i,v))
        let newBlock () =
            [ for i in 0 .. 2 -> String.replicate 9 "-" ]
            |> String.concat "+" |> printfn "+%s+"
        newBlock ()
        let index = ref 0
        for x in 0 .. 8 do 
            printf "|"
            for y in 0 .. 8 do                
                printf " %d " state.[!index]// (state' |> List.find(fun (i, v) -> i = !index) |> snd)
                index := !index + 1
                if y % 3 = 2 then printf "|"
            printfn ""
            if x % 3 = 2 then newBlock()


    sudokuSolver sudokuPuzzle2 = solution
    sudokuSolver sudokuPuzzle

    printState sudokuPuzzle
    printState (sudokuPuzzle |> sudokuSolver)


    printState sudokuPuzzle2
    printState (sudokuPuzzle2 |> sudokuSolver)


    let (|Found|_|) key map =
          map
          |> Map.tryFind key
          |> Option.map (fun x -> x, Map.remove key map)

    let map = Map.ofList [1, "A"; 2, "B"]

    match map with
    | Found 1 (x, rest) -> printfn "Value: %A, Remaining: %A" x rest
    | _ -> ()


        // concatenate a list of strings 
    // into single string 
    let rec conactStringList = 
       function head :: tail -> head + conactStringList tail 
                | [] -> ""// test data 

    let jabber = 
       ["'Twas "; "brillig, "; "and ";
          "the "; "slithy "; "toves "; "..."] 
    // call the function 
    let completJabber = conactStringList jabber 
    // print the result 
    printfn "%s" completJabber


    type SudokuSolver(lst:int seq) =
    
            member x.Solve() = sudokuSolver (lst |> Seq.toList) |> List.toSeq

