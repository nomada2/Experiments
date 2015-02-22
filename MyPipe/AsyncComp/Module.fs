module AsyncModule

type Parallel<'T> =
    private {   Compute : Async<obj>[]
                Unpack : obj [] -> int -> 'T }

    static member ( <*> ) (f: Parallel<'A -> 'B>, x: Parallel<'A>) : Parallel<'B> =
        {
            Compute = Array.append f.Compute x.Compute
            Unpack = fun xs pos ->
                            let fv = f.Unpack xs pos
                            let xv = x.Unpack xs (pos + f.Compute.Length)
                            fv xv   }

    static member ( <*> ) (f: Parallel<'A ->' B>, x: Async<'A>) : Parallel<'B> =
            f <*> Parallel.Await(x)

and Parallel =
    static member Run<'T> (p: Parallel<'T>) : Async<'T> =
        async { let! results =
                    match p.Compute.Length with
                    | 0 -> async.Return [|box ()|]
                    | 1 -> async { let! r = p.Compute.[0] 
                                   return [| r |] }
                    | _ -> Async.Parallel p.Compute
                return p.Unpack results 0   }

    static member Await<'T> (x: Async<'T>) : Parallel<'T> =
        {
            Compute =   [|  async { let! v = x
                                    return box v    } |]
            Unpack = fun xs pos -> unbox xs.[pos]
        }

    static member Pure<'T>(x: 'T) : Parallel<'T> =
        {   Compute = [||]
            Unpack = fun _ _ -> x   }

let myInt, myChar, myBool, myString =
    Parallel.Pure (fun w x y z -> (w, x, y, z))
    <*> async { return 1 }
    <*> async { return 'b' }
    <*> async { return true }
    <*> async { return "abc" }
    |> Parallel.Run
    |> Async.RunSynchronously

let arrAsync = [|   async { printfn "1 S"
                            do! Async.Sleep 3000
                            printfn "1 E"
                            return 1 }
                    async { printfn "2 S"
                            do! Async.Sleep 500
                            printfn "2 E"
                            return 2 }
                    async { printfn "3 S"
                            do! Async.Sleep 1000
                            printfn "3 E"                
                            return 3 }
                    async { printfn "4 S"
                            do! Async.Sleep 2000
                            printfn "4 E"
                            return 4 } |]

arrAsync |> Async.Parallel |> Async.Ignore |> Async.Start

let test1 () =
    Parallel.Pure (fun x y z -> (x, y, z))
    <*> async { return 1 }
        // unit -> Parallel<('b -> 'c -> int * 'b * 'c)>

    <*> async { return 'b' }
        // unit -> Parallel<('c -> int * char * 'c)>

    <*> async { return true }
        // unit -> Parallel<int * char * bool>

    |> Parallel.Run
        // unit -> Async<int * char * bool>

    |> Async.RunSynchronously
        // unit -> int * char * bool

let a1 x = async { return x + x }
let a2 x = async { return x * x }
let a3 x = async { return string (x * x) }
let a4 (x:string) = async { return "Riccardo is cool" }
let a5 (x:bool) = async { return not <| x }

let operations = async {
    
        let! x1 = Async.StartChild(a1 2)
        let! x2 = Async.StartChild(a2 2)
        let! x3 = Async.StartChild(a3 2)
        let! x4 = Async.StartChild(a4 "ciao")
        let! x5 = Async.StartChild(a5 true)
        
        let! x1' = x1
        let! x2' = x2
        let! x3' = x3
        let! x4' = x4
        let! x5' = x5

        return x1', x2', x3', x4', x5'
    }

operations |> Async.RunSynchronously |> printfn "%A"

type Message<'a, 'b> =
    | Process of ('a -> Async<'b>) * 'a * AsyncReplyChannel<'b>

let agent = MailboxProcessor.Start(fun inbox ->
                let rec loop n = async {
                    let! msg = inbox.Receive()
                    match msg with
                    | Process (f, a, reply) ->  let! r = Async.StartChild(f a)
                                                let! r' = r
                                                reply.Reply(r')
                    return! loop(n + 1)
                }
                loop 0)

let p1 = agent.PostAndAsyncReply(fun reply -> Process(a1, 3, reply)) |> Async.RunSynchronously
//  let p2 = agent.PostAndAsyncReply(fun reply -> Process(a4, "ciao", reply)) |> Async.RunSynchronously

type ThrottlingMessage = 
  //| Enqueue of Async<unit>
  | Enqueue of Async<obj> * AsyncReplyChannel<obj>
  | Completed

let throttlingAgent limit = MailboxProcessor.Start(fun inbox -> async {
  let queue = System.Collections.Generic.Queue<_>()
  let running = ref 0
  while true do
    let! msg = inbox.Receive()
    match msg with
    | Completed -> decr running
    //| Enqueue w -> queue.Enqueue(w)
    | Enqueue (work, reply) -> queue.Enqueue(work, reply)
    while running.Value < limit && queue.Count > 0 do
      let (work, reply) = queue.Dequeue()
      incr running
      do! 
        async { let! r = work
                reply.Reply(r) 
                inbox.Post(Completed) } 
        |> Async.StartChild
        |> Async.Ignore })

let w = throttlingAgent 5 
for i in 0 .. 20 do 
  async { printfn "Starting %d" i
          do! Async.Sleep(1000)
          printfn "Done %d" i  }
  |> Enqueue
  |> w.Post
  

let pack = fun xs -> box xs
let unpack = fun xs -> unbox xs

let compute (f:'a -> 'b) (arg:'a) :'b = 
        let b = f(arg) |> pack
        unpack b

compute (fun x -> x + x) 2

compute (fun x -> string (x + x)) 2


let computeAsync (x: Async<'T>) =
            async { let! v = x
                    return box v }

let unpackAsync (x:obj) :'T = unpack x

let agentTr = throttlingAgent 2

let computeAsync (f:'a -> 'b) (arg:'a) :'b = 
        async {     let b = async { let b' = f(arg) 
                                    return box b' }
                    let! result = agentTr.PostAndAsyncReply(fun reply -> Enqueue(b, reply))
                    return unpack result } |> Async.RunSynchronously

let computeAsync2 (f:'a -> 'b) (arg:'a) = 
       let b = async { let b' = f(arg) 
                       return box b' }
       Async.StartWithContinuations(agentTr.PostAndAsyncReply(fun reply -> Enqueue(b, reply)),
                                    (fun res -> let result = unpack res
                                                printfn "result %A" result
                                                ()),
                                    (fun exn -> ()),
                                    (fun cnl -> ()))




let a1 x = x + x 
let a2 x = x * x 
let a3 x = string (x * x) 
let a4 (x:string) = "Riccardo is cool" 
let a5 (x:bool) = not <| x 

computeAsync2 a1 2    
computeAsync2 a2 2    
computeAsync2 a3 2    
computeAsync2 a4 "ciao"
computeAsync2 a5 true
         
    

//type Parallel<'T> =
//    private {   Compute : Async<obj>[]
//                Unpack : obj [] -> int -> 'T }
//
//    static member Await<'T> (x: Async<'T>) : Parallel<'T> =
//        {
//            Compute =   [|  async { let! v = x
//                                    return box v    } |]
//            Unpack = fun xs pos -> unbox xs.[pos]
//        }


module Async =

    let  sendToTPL task = 
         Async.StartAsTask <| async { return task }

    /// Run two async's in parallel in the thread pool and return the results together, asynchronously
    let Parallel2 (p1, p2) = 
        async { let! job1 = Async.StartChild p1
                let! job2 = Async.StartChild p2
                let! res1 = job1
                let! res2 = job2
                return (res1, res2) }

    /// Run three async's in parallel in the thread pool and return the results together, asynchronously
    let Parallel3 (p1, p2, p3) = 
        async { let! job1 = Async.StartChild p1
                let! job2 = Async.StartChild p2
                let! job3 = Async.StartChild p3
                let! res1 = job1
                let! res2 = job2
                let! res3 = job3
                return (res1, res2, res3) }

    let private boxp p = async { let! res = p in return box res }

    /// Alternative version of Async.Parallel2
    let Parallel2b (p1:Async<'T1>, p2:Async<'T2>) : Async<'T1 * 'T2> = 
        async { let! results = Async.Parallel [| boxp p1; boxp p2 |]
                return (unbox results.[0],unbox results.[1]) }

    /// Alternative version of Async.Parallel3
    let Parallel3b (p1:Async<'T1>, p2:Async<'T2>, p3:Async<'T3>) : Async<'T1 * 'T2 * 'T3> = 
        let boxp p = async { let! res = p in return box res }
        async { let! results = Async.Parallel [| boxp p1; boxp p2; boxp p3 |]
                return (unbox results.[0], unbox results.[1], unbox results.[2]) }


module Fsnip =
    open System

    type Microsoft.FSharp.Control.Async with
      /// Starts the specified operation using a new CancellationToken and returns
      /// IDisposable object that cancels the computation. This method can be used
      /// when implementing the Subscribe method of IObservable interface.
      static member StartDisposable(op:Async<unit>) =
        let ct = new System.Threading.CancellationTokenSource()
        Async.Start(op, ct.Token)
        { new IDisposable with 
            member x.Dispose() = ct.Cancel() }

    /// Creates IObservable that fires numbers with specified delay
    let createCounterEvent (delay) =

      /// Recursive async computation that triggers observer
      let rec counter (observer:IObserver<_>) count = async {
        do! Async.Sleep(delay)
        observer.OnNext(count)
        return! counter observer (count + 1) }

      // Return new IObservable that starts 'counter' using
      // 'StartDisposable' when a new client subscribes.
      { new IObservable<_> with
          member x.Subscribe(observer) =
            counter observer 0
            |> Async.StartDisposable }

    // Start the counter with 1 second delay and print numbers
    let disp = 
      createCounterEvent 1000
      |> Observable.map (sprintf "Count: %d")
      |> Observable.subscribe (printfn "%s")

    disp.Dispose()


//////////////////////////////////

type JobMessage = 
    DoJob of Async<unit>
    | DoneJob
    | Quit of AsyncReplyChannel<unit>

let Worker () = 
    MailboxProcessor<JobMessage>.Start(fun mbox -> 
        let rec acceptingPhase pending  = 
            async {
                let! m = mbox.Receive() 
                match m with
                | DoJob j -> async { do! j 
                                     mbox.Post(DoneJob) 
                                   } |> Async.Start 
                             do! acceptingPhase (pending+1)
                | DoneJob -> do! acceptingPhase (pending-1)
                | Quit reply -> do! quitPhase reply pending
            }
        and quitPhase reply pending = 
            async {
                if pending <= 0 then reply.Reply()
                else
                    let! m = mbox.Receive() 
                    match m with 
                    | DoneJob -> do! quitPhase reply (pending-1)
                    | _ -> do! quitPhase reply pending
            }
        acceptingPhase 0
        )

let w = Worker()

[0..20]
|> Seq.iteri(fun i j -> let job = async {   printfn "starting %d" i
                                            do! Async.Sleep 500
                                            printfn "ending %i" i }
                        w.Post(DoJob job) )



type Actor = MailboxProcessor<obj>
type Ident = string
type Cond = Actor
type Env = Actor
 
 
let (<!>) (actor : Actor) (msg : 'T) = actor.Post msg
 
// run forever - template
let start<'T> (work : 'T -> unit) : Actor =
    MailboxProcessor<obj>.Start(fun mb ->
        let rec loop () =
            async {
                let! msg = mb.Receive()
                match msg with
                | :? 'T as msg' -> work msg'
                | _ -> () // oops... undefined behaviour
                return! loop ()
            }
        loop () )
 
// Helper print expression
let printExp = start<obj>(fun value -> printfn "Print: %A" value)

printExp <!> "ciao"

//////////////////////


module Observable =

    open System
    open System.Reactive
    open System.Reactive.Linq

    /// Generates an observable from an Async<_>.
    let fromAsync computation = 
      Observable.Create<'a>(Func<IObserver<'a>,Action>(fun o ->
        if o = null then nullArg "observer"
        let cts = new System.Threading.CancellationTokenSource()
        let invoked = ref 0
        let cancelOrDispose cancel =
          if System.Threading.Interlocked.CompareExchange(invoked, 1, 0) = 0 then
            if cancel then cts.Cancel() else cts.Dispose()
        let wrapper = async {
          try
            let res = ref Unchecked.defaultof<_>
            try
              let! result = computation
              res := result
            with e -> o.OnError(e)
            o.OnNext(!res)
            o.OnCompleted()
          finally cancelOrDispose false }
        Async.StartImmediate(wrapper, cts.Token)
        Action(fun () -> cancelOrDispose true)))

    let asyncOp =  async { return 1}
    let obs = fromAsync asyncOp
    let idsp = obs.Subscribe(new Action<_>((fun x -> printfn "obs ops %A" x)))
    
