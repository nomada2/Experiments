namespace Async

module DAG =
   
    open System
    open System.Collections.Generic
    open System.Threading
    open Microsoft.FSharp.Collections

    type MList<'a> = System.Collections.Generic.List<'a>

    type TaskMessage =
        | AddTask of int * TaskInfo
        | QueueTask of TaskInfo
        | ExecuteTasks
    and TaskInfo =
        { Context : System.Threading.ExecutionContext  
          Edges : int array
          Id : int
          Task : unit -> unit
          NumRemainingEdges : int option
          Start : DateTimeOffset option
          End : DateTimeOffset option }
            
    type ParallelTasksDAG() =

        let onTaskCompleted = new Event<TaskInfo>()
            
        let verifyThatAllOperationsHaveBeenRegistered (tasks:Dictionary<int, TaskInfo>) =
            let tasksNotRegistered =           
                tasks.Values                 
                |> (Seq.collect (fun f -> f.Edges) >> set)
                |> Seq.filter(tasks.ContainsKey >> not)
            if tasksNotRegistered |> Seq.length > 0 then
                let edgesMissing = tasksNotRegistered |> Seq.map (string) |> Seq.toArray 
                raise (InvalidOperationException(sprintf "Missing operation: %s" (String.Join(", ", edgesMissing))))

        let verifyTopologicalSort(tasks:Dictionary<int, TaskInfo>) =
            // Build up the dependencies graph
            let tasksToFrom = new Dictionary<int, MList<int>>(tasks.Values.Count, HashIdentity.Structural)
            let tasksFromTo = new Dictionary<int, MList<int>>(tasks.Values.Count, HashIdentity.Structural)

            for op in tasks.Values do
                // Note that op.Id depends on each of op.Edges
                tasksToFrom.Add(op.Id, new MList<int>(op.Edges))
                // Note that each of op.Dependencies is relied on by op.Id
                for deptId in op.Edges do
                    let success, _ = tasksFromTo.TryGetValue(deptId) 
                    if not <| success then tasksFromTo.Add(deptId, new MList<int>())
                    tasksFromTo.[deptId].Add(op.Id)
            // Create the sorted list
            let partialOrderingIds = new MList<int>(tasksToFrom.Count)
            let iterationIds = new MList<int>(tasksToFrom.Count)
                        
            let rec buildOverallPartialOrderingIds() =  
                match tasksToFrom.Count with
                | 0 -> Some(partialOrderingIds)
                | _ ->  iterationIds.Clear()
                        for item in tasksToFrom do
                            if item.Value.Count = 0 then
                                iterationIds.Add(item.Key)
                                let success, depIds = tasksFromTo.TryGetValue(item.Key)
                                if success = true then
                                    // Remove all outbound edges
                                    for depId in depIds do                                  
                                        tasksToFrom.[depId].Remove(item.Key) |> ignore
                        // If nothing was found to remove, there's no valid sort.
                        if iterationIds.Count = 0 then None
                        else
                            // Remove the found items from the dictionary and 
                            // add them to the overall ordering
                            for id in iterationIds do
                                tasksToFrom.Remove(id) |> ignore
                            partialOrderingIds.AddRange(iterationIds)
                            buildOverallPartialOrderingIds()            
            buildOverallPartialOrderingIds()
            
        let verifyThereAreNoCycles(operations:Dictionary<int, TaskInfo>) = 
            if verifyTopologicalSort(operations) = None then
                raise (InvalidOperationException("Cycle detected"))

        let nrd =   function
            | Some(n) -> Some(n - 1)
            | None -> None     
                   
        let rec getDependentOperation (dep : int list) (ops : Dictionary<int, TaskInfo>) acc =
            match dep with
            | [] -> acc
            | h :: t ->     ops.[h] <- { ops.[h] with NumRemainingEdges = nrd ops.[h].NumRemainingEdges }
                            match ops.[h].NumRemainingEdges.Value with
                            | 0 ->  getDependentOperation t ops (ops.[h] :: acc)
                            | _ ->  getDependentOperation t ops acc

                    
        let dagAgent =
            let inbox = new MailboxProcessor<TaskMessage>(fun inbox ->
                let rec loop (tasks : Dictionary<int, TaskInfo>) 
                             (edges : Dictionary<int, int list>) = async {
                        let! msg = inbox.Receive()
                        match msg with
                        | ExecuteTasks ->
                            // Verify that all operations are registered
                            verifyThatAllOperationsHaveBeenRegistered(tasks)
                            // Verify no cycles
                            verifyThereAreNoCycles(tasks)

                            let dependenciesFromTo = new Dictionary<int, int list>()
                            let operations' = new Dictionary<int, TaskInfo>()

                            // Fill dependency data structures
                            for KeyValue(key, value) in tasks do
                                let operation' =
                                    { value with NumRemainingEdges = Some(value.Edges.Length) }
                                for from in operation'.Edges do
                                    let exists, lstDependencies = dependenciesFromTo.TryGetValue(from)
                                    if not <| exists then 
                                        dependenciesFromTo.Add(from, [ operation'.Id ])
                                    else
                                        dependenciesFromTo.[from] <- (operation'.Id :: lstDependencies)
                                operations'.Add(key, operation')
                           
                            
                            operations' |> Seq.filter (fun kv ->
                                                   match kv.Value.NumRemainingEdges with                                                 
                                                   | Some(n) when n = 0 -> true
                                                   | _ -> false)
                                        |> Seq.iter (fun op -> inbox.Post(QueueTask(op.Value)))
                            return! loop operations' dependenciesFromTo
                     
                        | QueueTask(op) ->
                                Async.Start <| async { 
                                    // Time and run the operation's delegate
                                    let start' = DateTimeOffset.Now
                                    match op.Context with
                                    | null -> op.Task()
                                    | ctx ->
                                        ExecutionContext.Run(ctx.CreateCopy(),
                                                                (fun op -> let opCtx = (op :?> TaskInfo)
                                                                           (opCtx.Task())), op)
                                    let end' = DateTimeOffset.Now
                                    // Raise the operation completed event
                                    onTaskCompleted.Trigger  { op with Start = Some(start')
                                                                       End = Some(end') }
                                    
                                    // Queue all the operations that depend on the completation 
                                    // of this one, and potentially launch newly available
                                    let exists, lstDependencies = edges.TryGetValue(op.Id)
                                    if exists && lstDependencies.Length > 0 then
                                        let dependentOperation' = getDependentOperation lstDependencies tasks []
                                        edges.Remove(op.Id) |> ignore
                                        dependentOperation'
                                            |> Seq.iter (fun nestedOp -> inbox.Post(QueueTask(nestedOp))) }
                                return! loop tasks edges
                      
                        | AddTask(id, op) -> tasks.Add(id, op)
                                             return! loop tasks edges
                    }
                loop (new Dictionary<int, TaskInfo>(HashIdentity.Structural)) (new Dictionary<int, int list>(HashIdentity.Structural)))
            inbox.Error |> Observable.add(fun ex -> printfn "Error : %s" ex.Message )
            inbox.Start()
            inbox
            

        [<CLIEventAttribute>]
        member this.OnTaskCompleted = onTaskCompleted.Publish

        member this.ExecuteTasks() = dagAgent.Post ExecuteTasks
      
        member this.AddTask(id, task, [<ParamArrayAttribute>] edges : int array) =
            let data =
                { Context = ExecutionContext.Capture()
                  Edges = edges
                  Id = id
                  Task = task
                  NumRemainingEdges = None
                  Start = None
                  End = None }
            dagAgent.Post(AddTask(id, data))


  