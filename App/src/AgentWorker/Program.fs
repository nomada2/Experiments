open System
open System
open System.Collections.Generic

type Agent<'a> = MailboxProcessor<'a>


type BlockingAgentMessage<'T> = 
  | Add of 'T //* AsyncReplyChannel<unit> 
  | Get of AsyncReplyChannel<ThrottlingAgentMessage<'T>>

/// Message type used by the agent - contains queueing 
/// of work items and notification of completion 
and  ThrottlingAgentMessage<'T> = 
  | Completed
  | Work of 'T

type UnboundedQueueQgent<'T>() =    
    let agent = Agent.Start(fun agent ->
        let queue = new Queue<'T>()

        let rec emptyQueue() = 
          agent.Scan(fun msg ->
            match msg with 
            | Add(value) -> Some(enqueueAndContinue(value))
            | _ -> None )
        and runningQueue() = async {
          let! msg = agent.Receive()
          match msg with 
          | Add(value) -> return! enqueueAndContinue(value)
          | Get(reply) -> return! dequeueAndContinue(reply) }

        and enqueueAndContinue(value) = async {
          queue.Enqueue(value)
          return! chooseState() }
        and dequeueAndContinue (reply) = async {
          let item = queue.Dequeue()
          reply.Reply(Work(item))
          return! chooseState() }
        and chooseState() = 
          if queue.Count = 0 then emptyQueue()
          else runningQueue()
        emptyQueue() )

    member x.AsyncAdd(v:'T) = 
        agent.Post(Add(v))

    member x.AsyncGet() = 
        agent.PostAndAsyncReply(fun r -> Get(r))

type BlockingQueueAgent<'T, 'U>(action:'T -> Async<'U>, limit) =

  let agentInput = new UnboundedQueueQgent<'T>()
  let agentOutput = new UnboundedQueueQgent<'U>()

  let trottlingagent = Agent.Start(fun tagent -> 

    /// Represents a state when the agent is blocked
    let rec waiting () = 
      // Use 'Scan' to wait for completion of some work
      tagent.Scan(function
        | Completed -> Some(working (limit - 1))
        | _ -> None)

    and working count = async { 
      let! (Work(work)) = agentInput.AsyncGet()  
      async { try let! result = action(work)
                  agentOutput.AsyncAdd(result)
              finally tagent.Post(Completed) } |> Async.Start
      if count < limit - 1 then return! working (count + 1)
      else return! waiting () }
    working 0)      
     
  member x.AsyncAdd(v:'T) = 
    agentInput.AsyncAdd(v)

  member x.AsyncGet() = 
    agentOutput.AsyncGet()


let b = BlockingQueueAgent<int, int>((fun x -> async{ printfn "ciao %A" x
                                                      do! Async.Sleep x
                                                      return x }), 2)

b.AsyncAdd(1000)
b.AsyncAdd(500)
b.AsyncAdd(2000)
b.AsyncAdd(1500)

  /// Queue the specified asynchronous workflow for processing
//  member x.DoWork(work) = agent.Post(Work work)

let getItem() = b.AsyncGet()

Async.StartWithContinuations(getItem(), 
                (fun res -> printfn "Result is %A" res),
                (fun _ -> ()),
                (fun _ -> ()))


let add n = 
        for i = 0 to n do b.AsyncAdd(i)
 
let get n = seq {
        for i = 0 to n do yield b.AsyncGet() }
     
System.Threading.Tasks.Task.Factory.StartNew(new Action(fun _ -> add 1000))

Async.Parallel( (get 500) ) |> Async.RunSynchronously


//System.Threading.Tasks.Task.Factory.StartNew(new Action(fun _ -> get 500))



           
(*


let bAgent = BlockingQueueAgent<int>(5)

bAgent.AsyncAdd(5) |> Async.Start

Async.StartWithContinuations(bAgent.AsyncGet(),
                             (fun r -> printfn "Item %d" r),
                             (fun _ -> ()),
                             (fun _ -> ()))



// [snippet:Demo]
// To use the throttling agent, call it with a specified limit
// and then add items using the 'Enqueue' message!
let w = throttlingAgent 5 
for i in 0 .. 20 do 
  async { printfn "Starting %d" i
          do! Async.Sleep(1000)
          printfn "Done %d" i  }
  |> Enqueue
  |> w.Post
*)


[<EntryPoint>]
let main argv = 

    for a in argv do
        printfn "%A" a

    printfn "Completed!"
    Console.ReadLine() |> ignore

    0 // return an integer exit code

