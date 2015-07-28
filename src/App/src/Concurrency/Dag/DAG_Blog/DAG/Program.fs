open System

[<EntryPoint>]
let main argv = 

    let cprintf c fmt = 

        Printf.kprintf 
            (fun s -> 
                let old = System.Console.ForegroundColor 
                try 
                  System.Console.ForegroundColor <- c;
                  System.Console.Write s
                finally
                  System.Console.ForegroundColor <- old) 
            fmt
            
    // Colored printfn
    let cprintfn c fmt = 
        cprintf c fmt
        printfn ""

    let acc1() = printfn "action 1"; System.Threading.Thread.Sleep 2000; //printfn "action 1 completed"
    let acc2() = printfn "action 2"; System.Threading.Thread.Sleep 3000;// printfn "action 2 completed"
    let acc3() = printfn "action 3"
    let acc4() = printfn "action 4"; System.Threading.Thread.Sleep 5000; //printfn "action 4 completed"
    let acc5() = printfn "action 5"
    let acc6() = printfn "action 6"
    let acc7() = printfn "action 7"
    let acc8() = printfn "action 8"
    let acc9() = printfn "action 9"
    let acc10() = printfn "action 10"
    let acc11() = printfn "action 11"
    let acc12() = printfn "action 12"

    let dagAsync = Async.DAG.ParallelTasksDAG()
    dagAsync.OnTaskCompleted |> Observable.add(fun op ->  System.Console.ForegroundColor <- ConsoleColor.Green 
                                                          printfn "Completed %d" op.Id)

    dagAsync.AddTask(1, acc1, 4,5)
    dagAsync.AddTask(2, acc2, 5)
    dagAsync.AddTask(3, acc3, 6, 5)
    dagAsync.AddTask(4, acc4, 6)
    dagAsync.AddTask(5, acc5, 7, 8)
    dagAsync.AddTask(6, acc6, 7)
    dagAsync.AddTask(7, acc7)
    dagAsync.AddTask(8, acc8)
    dagAsync.ExecuteTasks()
    
    System.Console.ReadLine() |> ignore
    //cprintfn ConsoleColor.Red "Starting version 1" 
    let dagManager = Async.DAG.ParallelTasksDAG()

    dagManager.OnTaskCompleted |> Observable.add(fun op ->  System.Console.ForegroundColor <- ConsoleColor.Magenta 
                                                            printfn "Completed %d" op.Id)

    dagManager.OnTaskCompleted |> Observable.scan(fun u t -> t.Id + u) 0 
                           |> Observable.add(fun op ->   if op = 78 then
                                                            System.Console.ForegroundColor <- ConsoleColor.Blue
                                                            printfn "Total sum %d" op)
    //dagManager.AddOperation(12, acc11, 14)
   // dagManager.Execute()

    //  action 3
    //  action 11
    //  action 1
    //  action 1 completed
    //  action 6
    //  action 7
    //  action 2
    //  action 2 completed
    //  action 8
    //  action 4
    //  action 4 completed
    //  action 5
    //  action 10
    //  action 9

    //System.Console.ReadLine() |> ignore

    //dagManager.OnOperationComplete.Add(fun op -> printfn "Completed %d" op.Id)
    dagManager.AddTask(1, acc1, 3)
    //dagManager.AddOperation(1, acc1, 2)
       
    dagManager.AddTask(2, acc2, 1)
    dagManager.AddTask(3, acc3)
    dagManager.AddTask(4, acc4, 2, 3)   
    dagManager.AddTask(5, acc5, 3, 4)
    dagManager.AddTask(6, acc6, 1, 3)
    dagManager.AddTask(7, acc7, 1)
    dagManager.AddTask(8, acc8, 2, 3)
    dagManager.AddTask(9, acc9, 1, 4, 7)
    dagManager.AddTask(10, acc10, 2, 4, 7)
    dagManager.AddTask(11, acc11)
    dagManager.AddTask(12, acc12, 7,4)
    //dagManager.AddOperation(12, acc11, 14)
    dagManager.ExecuteTasks()
    //cprintfn ConsoleColor.Green "Completed version 1" 

    let rnd = Random()
    let rndTime() = System.Threading.Thread.Sleep (rnd.Next(20, 1000))

    let old = System.Console.ForegroundColor 

    cprintfn ConsoleColor.Yellow "Starting version 2" 
    let dm = Async.DAG.ParallelTasksDAG()
    dm.OnTaskCompleted |> Observable.add(fun op ->  System.Console.ForegroundColor <- ConsoleColor.Green 
                                                    printfn "Completed %d" op.Id)

    dm.OnTaskCompleted |> Observable.scan(fun u t -> t.Id + u) 0 
                           |> Observable.add(fun op -> if op = 66 then
                                                            System.Console.ForegroundColor <- ConsoleColor.Red
                                                            printfn "Total sum %d" op)
    dm.AddTask(1,rndTime)
    dm.AddTask(2,rndTime)
    dm.AddTask(3,rndTime, 11)
    dm.AddTask(4,rndTime, 1,5)
    dm.AddTask(5,rndTime, 1,2, 3)
    dm.AddTask(6,rndTime, 5,3)
    dm.AddTask(7,rndTime, 3, 4)
    dm.AddTask(8,rndTime, 5, 6)
    dm.AddTask(9,rndTime, 5)
    dm.AddTask(10, rndTime, 5,9)
    dm.AddTask(11, rndTime)
    dm.ExecuteTasks()

    //1 - 2 - 3 - 11
    //5
    //6 - 8 - 4 - 9
    //10 - 7

    System.Console.ReadLine() |> ignore
    0