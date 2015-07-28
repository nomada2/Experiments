open System
open System.IO
//open System.Collections
open System.Collections.Generic
open System.Collections.Concurrent
open System.Threading.Tasks
 
type Pool private () =
    let queue = new BlockingCollection<_>(ConcurrentBag())
    let work() =
        while true do
            queue.Take()()
    let long = TaskCreationOptions.LongRunning
    let task = Task.Factory.StartNew(work, long)
    static let self = Pool()
    member private this.Add f = queue.Add f
    static member Spawn(f:unit -> unit) = self.Add f
 
type internal ChannleCommand<'a> =
    | Read of ('a -> unit) * AsyncReplyChannel<unit>
    | Write of 'a * (unit -> unit) * AsyncReplyChannel<unit>
 
[<Sealed>]
type ChannelAgent<'a>() =
  
    let agent = MailboxProcessor.Start(fun inbox ->
        let readers = Queue()
        let writers = Queue()
 
        let rec loop() = async {
            let! msg = inbox.Receive()
            match msg with
            | Read(ok , reply) ->
                if writers.Count = 0 then
                    readers.Enqueue ok
                    reply.Reply( () )
                else
                    let (value, cont) =writers.Dequeue()
                    Pool.Spawn cont
                    reply.Reply( (ok value) )

                return! loop()
            | Write(x, ok, reply) ->
                if readers.Count = 0 then
                    writers.Enqueue(x, ok)
                    reply.Reply( () )
                else
                    let cont = readers.Dequeue()
                    Pool.Spawn ok
                    reply.Reply( (cont x) )
                return! loop() }
        loop())
 
  
    member this.Read(ok: 'a -> unit)  =  // ('a -> unit) -> unit
        agent.PostAndAsyncReply(fun f -> Read(ok, f)) |> Async.RunSynchronously
       
    member this.Write(x: 'a, ok:unit -> unit)  = 
        agent.PostAndAsyncReply(fun f -> Write(x, ok, f)) |> Async.RunSynchronously 

    member this.Read() =
        Async.FromContinuations(fun (ok, _,_) -> agent.PostAndAsyncReply(fun f -> Read(ok, f)) |> Async.RunSynchronously)
 
    member this.Write (x:'a) = 
        Async.FromContinuations(fun (ok, _,_) -> 
            agent.PostAndAsyncReply(fun f -> Write(x, ok, f)) |> Async.RunSynchronously )
(*    
 
  member this.Read() : Async<'a>= //async.Return (Unchecked.defaultof<'a>)
        Async.FromContinuations(fun (ok, _,_) ->
            agent.PostAndAsyncReply(fun f -> Read(ok, f)) )


    member inline this.Write (x:'a) :Async<unit> = 
        agent.PostAndAsyncReply(fun f -> Write(x, ok, f))
 *)

[<Sealed>]
type Channel<'a>() =
    let readers = Queue()
    let writers = Queue()
 
    member this.Read ok =
        let task =
            lock readers <| fun () ->
                if writers.Count = 0 then
                    readers.Enqueue ok
                    None
                else
                    Some (writers.Dequeue())
       
        match task with
        | None -> ()
        | Some(value, cont) ->  Pool.Spawn cont
                                ok value
 
    member this.Write(x: 'a, ok) =
        let task =
            lock readers <| fun () ->
                if readers.Count = 0 then
                    writers.Enqueue(x, ok)
                    None
                else
                    Some(readers.Dequeue())
       
        match task with
        | None -> ()
        | Some cont ->
            Pool.Spawn ok
            cont x
   
    member inline this.Read() =
        Async.FromContinuations(fun (ok, _,_) -> this.Read ok)
 
    member inline this.Write x =
        Async.FromContinuations(fun (ok, _,_) -> this.Write(x, ok))
      
 
let test (n:int) =
    //let chan = ChannelAgent()
    let chan = Channel()
    let rec writer (i:int) =
        async {
            if i = 0 then
                return! chan.Write 0
            else
                do! chan.Write i
                return! writer (i - 1)
        }
    let rec reader sum =
        async {
        let! x = chan.Read()
        if x = 0 then
            return sum
        else
            return! reader (sum + x)
        }
    Async.Start(writer n)
 
    let clock = System.Diagnostics.Stopwatch()
    let r = Async.RunSynchronously( reader 0)
    printfn "Hops per second %f" (float n  / clock.Elapsed.TotalSeconds)
    r
 
 
 
[<EntryPoint>]
let main argv =
    
    let res = test(100000)
    printfn "Res %d" res
 
    Console.ReadLine() |> ignore
 
    0 // return an integer exit code

