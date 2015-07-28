  haskell
   factorial n = product [1..n]
   
module StreamExt =
   type Stream with
      member x.ToObservable(size) =
         Observable.Create(fun (observer: IObserver<_>) ->
            let buffer = Array.zeroCreate size
            let defered = Observable.Defer(fun () -> (x.ReadAsync (buffer, 0, size)).ToObservable())
            Observable.Repeat<int>(defered)
                      .Select(fun i -> buffer.Take(i).ToArray())
                      .Subscribe( (fun (data:byte[]) -> if data.Length > 0 then observer.OnNext(data)
                                                        else observer.OnCompleted()), observer.OnError, observer.OnCompleted ))
                                                        
module StreamExt =
   type Stream with
      member x.ToObservable(size) =
         Observable.Create (fun (observer: IObserver<_>) ->
            let buffer = Array.zeroCreate size
            Observable.Defer(fun () -> (x.ReadAsync (buffer, 0, size)).ToObservable())
            |> Observable.repeat
            |> Observable.map(fun i -> buffer |> Seq.take i |> Seq.toArray)
            |> Observable.subscribe(function
                                    | data when data.Length > 0 -> observer.OnNext(data)
                                    | _ -> observer.OnCompleted()) observer.OnError observer.OnCompleted)


module Observable =
   ///Repeats the observable
   let repeat f = Observable.Repeat(source = f)

   /// maps the given observable with the given function
   let map f source = Observable.Select(source, Func<_,_>(f))

   /// Subscribes to the observable with all three callbacks
   let subscribe onNext onError onCompleted (observable: 'a IObservable) =
       observable.Subscribe(Observer.Create(Action<_> onNext, Action<_> onError, Action onCompleted))

///////////////////////


type RequestGate(n : int) =
    let semaphore = new Semaphore(initialCount = n, maximumCount = n)
    member x.AsyncAcquire(?timeout) =
        async {let! ok = Async.AwaitWaitHandle(semaphore,
                                               ?millisecondsTimeout = timeout)
               if ok then
                   return
                     {new System.IDisposable with
                         member x.Dispose() =
                             semaphore.Release() |> ignore}
               else
                   return! failwith "couldn't acquire a semaphore" }


let using (ie : #System.IDisposable) f =
    try f(ie)
    finally ie.Dispose()
//val using : ie:'a -> f:('a -> 'b) -> 'b when 'a :> System.IDisposable



// ----------------------------------------------------------------------------

type private CircularBuffer<'T> (bufferSize:int) =
    let buffer = Array.zeroCreate<'T> bufferSize
    let mutable index = 0
    let mutable total = 0
    member this.Add value =
        if bufferSize > 0 then
            buffer.[index] <- value
            index <- (index + 1) % bufferSize
            total <- min (total + 1) bufferSize
    member this.Iter f =     
        let start = if total = bufferSize then index else 0
        for i = 0 to total - 1 do 
            buffer.[(start + i) % bufferSize] |> f

type private BufferAgentMessage<'T> =
    | Add of IObserver<'T>
    | Remove of IObserver<'T>
    | Next of 'T
    | Completed
    | Error of exn

module private BufferAgent =
    let start (bufferSize:int) =
        let subscribers = LinkedList<_>()
        let buffer = CircularBuffer bufferSize
        MailboxProcessor.Start(fun inbox ->
            let rec loop () = async {
                let! message = inbox.Receive()
                match message with
                | Add observer ->                   
                    subscribers.AddLast observer |> ignore
                    buffer.Iter observer.OnNext
                    return! loop ()
                | Remove observer ->
                    subscribers.Remove observer |> ignore
                    return! loop ()
                | Next value ->
                    for subscriber in subscribers do
                        subscriber.OnNext value
                    buffer.Add value
                    return! loop ()
                | Error e ->
                    for subscriber in subscribers do
                        subscriber.OnError e
                | Completed ->
                    for subscriber in subscribers do
                        subscriber.OnCompleted ()
            }
            loop ()
        )

[<Interface>]
type ISubject<'TIn,'TOut> =
    inherit System.IObserver<'TIn>
    inherit System.IObservable<'TOut>

type ReplaySubject<'T> (bufferSize:int) =
    let bufferSize = max 0 bufferSize
    let agent = BufferAgent.start bufferSize    
    let subscribe observer =
        observer |> Add |> agent.Post
        { new System.IDisposable with
            member this.Dispose () =
                observer |> Remove |> agent.Post
        }

    member this.OnNext value = Next value |> agent.Post
    member this.OnError error = Error error |> agent.Post
    member this.OnCompleted () = Completed |> agent.Post    
    member this.Subscribe(observer:System.IObserver<'T>) = subscribe observer

    interface ISubject<'T,'T> with
        member this.OnNext value = Next value |> agent.Post
        member this.OnError error = Error error |> agent.Post
        member this.OnCompleted () = Completed |> agent.Post
        member this.Subscribe observer = subscribe observer

and Subject<'T>() = inherit ReplaySubject<'T>(0)










module Async =
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


-----------------


let data = [|1L..1000000L|]

// [snippet: Reactive Extensions (Rx)]
open System
open System.Reactive.Linq

let rxValue =
   data
      .ToObservable()
      .Where(fun x -> x%2L = 0L)
      .Select(fun x -> x * x)
      .Sum()      
      .ToEnumerable()
      |> Seq.head
// Real: 00:00:02.702, CPU: 00:00:02.812, GC gen0: 121, gen1: 2, gen2: 1
// [/snippet]

// [snippet: Observable module]
(* [omit:Observable module extensions] *)
module Observable =
   open System
   let ofSeq (xs:'T seq) : IObservable<'T> =
      { new IObservable<'T> with
         member __.Subscribe(observer) =
            for x in xs do observer.OnNext(x)
            observer.OnCompleted()
            { new IDisposable with member __.Dispose() = ()}           
      }
   let inline sum (observable:IObservable< ^T >) : IObservable< ^T >
         when ^T : (static member ( + ) : ^T * ^T -> ^T) 
         and  ^T : (static member Zero : ^T) = 
      { new IObservable<'T> with 
         member this.Subscribe(observer:IObserver<'T>) =
            let acc = ref (LanguagePrimitives.GenericZero)
            let accumulator =
               { new IObserver<'T> with 
                  member __.OnNext(x) = acc := !acc + x
                  member __.OnCompleted() = observer.OnNext(!acc)
                  member __.OnError(_) = failwith "Not implemented"
               }
            observable.Subscribe(accumulator)
      }
   let first (observable:IObservable<'T>) : 'T =
      let value = ref (Unchecked.defaultof<'T>)
      let _ = observable.Subscribe(fun x -> value := x)
      !value
(* [/omit] *)

let obsValue =
   data
   |> Observable.ofSeq
   |> Observable.filter (fun x -> x%2L = 0L)
   |> Observable.map (fun x -> x * x)
   |> Observable.sum
   |> Observable.first
// Real: 00:00:00.458, CPU: 00:00:00.453, GC gen0: 18, gen1: 1, gen2: 0
// [/snippet]

// [snippet: Nessos Streams]
open Nessos.Streams

let streamValue =
   data
   |> Stream.ofArray
   |> Stream.filter (fun x -> x%2L = 0L)
   |> Stream.map (fun x -> x * x)
   |> Stream.sum
// Real: 00:00:00.119, CPU: 00:00:00.109, GC gen0: 0, gen1: 0, gen2: 0
// [/snippet]

==
Agent that can be used for controlling the number of concurrently executing a
synchronous workflows. The agent runs a specified number of operations concurrently a
nd queues remaining pending requests. The queued work items are started as soon as
 one of the previous items completes.


/// Message type used by the agent - contains queueing 
/// of work items and notification of completion 
type internal ThrottlingAgentMessage = 
  | Completed
  | Work of Async<unit>
    
/// Represents an agent that runs operations in concurrently. When the number
/// of concurrent operations exceeds 'limit', they are queued and processed later
type ThrottlingAgent(limit) = 
  let agent = MailboxProcessor.Start(fun agent -> 

    /// Represents a state when the agent is blocked
    let rec waiting () = 
      // Use 'Scan' to wait for completion of some work
      agent.Scan(function
        | Completed -> Some(working (limit - 1))
        | _ -> None)

    /// Represents a state when the agent is working
    and working count = async { 
      // Receive any message 
      let! msg = agent.Receive()
      match msg with 
      | Completed -> 
          // Decrement the counter of work items
          return! working (count - 1)
      | Work work ->
          // Start the work item & continue in blocked/working state
          async { try do! work 
                  finally agent.Post(Completed) }
          |> Async.Start
          if count < limit then return! working (count + 1)
          else return! waiting () }

    // Start in working state with zero running work items
    working 0)      

  /// Queue the specified asynchronous workflow for processing
  member x.DoWork(work) = agent.Post(Work work)




// Simple Async Observable Subject<'T> 
// based on MailboxProcessor. Type declaration is more ML like, but the idea is represented in a simple way!
    module Observable =
        open System
        open System.Collections.Generic

        module Subject =
            /// Subject state maintained inside of the mailbox loop
            module State =
                type t<'T> = {
                    observers : IObserver<'T> list
                    stopped   : bool
                }

                let empty() = {observers=[]; stopped=false}

            /// Messages required for the mailbox loop
            module Message =
                type t<'T> =
                | Add       of IObserver<'T>
                | Remove    of IObserver<'T>
                | Next      of 'T
                | Error     of exn
                | Completed

            /// Type t that implements IObservable<'T> and IObserver<'T>
            type t<'T>() =

                let error() = raise(new System.InvalidOperationException("Subject already completed"))

                let mbox = MailboxProcessor<Message.t<'T>>.Start(fun inbox ->
                    let rec loop(t:State.t<'T>) = async {
                        let! req = inbox.Receive()

                        match req with
                        | Message.Add(observer) ->
                            if not(t.stopped) then
                                return! loop ({t with observers = t.observers @ [observer]})
                            else error()

                        | Message.Remove(observer) ->
                            if not(t.stopped) then
                                let t = {t with observers = t.observers |> List.filter(fun f -> f <> observer)}
                                return! loop t
                            else error()

                        | Message.Next(value) ->
                            if not(t.stopped) then
                                t.observers |> List.iter(fun o -> o.OnNext(value))
                                return! loop t
                            else error()

                        | Message.Error(err) ->
                            if not(t.stopped) then
                                t.observers |> List.iter(fun o -> o.OnError(err))
                                return! loop t
                            else error()

                        | Message.Completed ->
                            if not(t.stopped) then
                                t.observers |> List.iter(fun o -> o.OnCompleted())
                                let t = {t with stopped = true}
                                return! loop t
                            else error()
                    }

                    loop (State.empty())
                )

                /// Raises OnNext in all the observers
                member x.Next value  = Message.Next(value)  |> mbox.Post
                /// Raises OnError in all the observers
                member x.Error ex    = Message.Error(ex)    |> mbox.Post
                /// Raises OnCompleted in all the observers
                member x.Completed() = Message.Completed    |> mbox.Post

                interface IObserver<'T> with
                    member x.OnNext value   = x.Next(value)
                    member x.OnError ex     = x.Error(ex)
                    member x.OnCompleted()  = x.Completed()

                interface IObservable<'T> with
                    member x.Subscribe(observer:IObserver<'T>) =
                        observer |> Message.Add |> mbox.Post
                        { new IDisposable with
                            member x.Dispose() =
                                observer |> Message.Remove |> mbox.Post }    



Extension to Control.Observable module to create an Observable linked to a MailboxProcessor. 
Messages posted to the mailbox are published to subscribers. Requires a cancelation token 

which when cancelled sends OnComplete to subscribers. Only the Post method is exposed from the 
internally created MailboxProcessor.                             



module Observable 
open System

let createObservableAgent<'T> (token:System.Threading.CancellationToken) =
    let finished = ref false
    let subscribers = ref (Map.empty : Map<int, IObserver<'T>>)

    let inline publish msg = 
        !subscribers 
        |> Seq.iter (fun (KeyValue(_, sub)) ->
            try
                    sub.OnNext(msg)
            with ex -> 
                System.Diagnostics.Debug.Write(ex))

    let completed() = 
        lock subscribers (fun () ->
        finished := true
        !subscribers |> Seq.iter (fun (KeyValue(_, sub)) -> sub.OnCompleted())
        subscribers := Map.empty)

    token.Register(fun () -> completed()) |> ignore //callback for when token is cancelled
            
    let count = ref 0
    let agent =
        MailboxProcessor.Start
            ((fun inbox ->
                async {
                    while true do
                        let! msg = inbox.Receive()
                        publish msg} ),
                token)
    let obs = 
        { new IObservable<'T> with 
            member this.Subscribe(obs) =
                let key1 =
                    lock subscribers (fun () ->
                        if !finished then failwith "Observable has already completed"
                        let key1 = !count
                        count := !count + 1
                        subscribers := subscribers.Value.Add(key1, obs)
                        key1)
                { new IDisposable with  
                    member this.Dispose() = 
                        lock subscribers (fun () -> 
                            subscribers := subscribers.Value.Remove(key1)) } }
    obs,agent.Post
(*
#load "ObservableExtensions.fs"
open System
let cts = new System.Threading.CancellationTokenSource()
type Data = {Value:string}

let observable,fPost = Observable.createObservableAgent<Data> cts.Token

let sub1 = 
    observable.Subscribe
        ({new IObserver<Data> with
            member x.OnNext msg = printfn "sub1 received msg %A" msg
            member x.OnError(e) = ()
            member x.OnCompleted() = printfn "sub1 received OnCompleted"
        })
let sub2 = 
    observable.Subscribe
        ({new IObserver<Data> with
            member x.OnNext msg = printfn "sub2 received msg %A" msg
            member x.OnError(e) = ()
            member x.OnCompleted() = printfn "sub2 received OnCompleted"
        })

for i in 1 .. 10 do fPost {Value=i.ToString()}

sub1.Dispose()

for i in 11 .. 14 do fPost {Value=i.ToString()}

cts.Cancel() //sends OnCompleted
*)


Observable.Subject
The Subject<T> type implements both IObserver<T> and IObservable<T>. It is 
functionally equivalent to the type of the same name in the Reactive Extensions (Rx) library.

module Observable

open System
open System.Collections.Generic

type Subject<'T> () =
   let sync = obj()
   let mutable stopped = false
   let observers = List<IObserver<'T>>()
   let iter f = observers |> Seq.iter f
   let onCompleted () =
      if not stopped then
         stopped <- true
         iter (fun observer -> observer.OnCompleted())
   let onError ex () =
      if not stopped then
         stopped <- true
         iter (fun observer -> observer.OnError(ex))
   let next value () =
      if not stopped then
         iter (fun observer -> observer.OnNext(value))
   let remove observer () =
      observers.Remove observer |> ignore
   member x.Next value = lock sync <| next value
   member x.Error ex = lock sync <| onError ex
   member x.Completed () = lock sync <| onCompleted
   interface IObserver<'T> with
      member x.OnCompleted() = x.Completed()
      member x.OnError ex = x.Error ex
      member x.OnNext value = x.Next value
   interface IObservable<'T> with
      member this.Subscribe(observer:IObserver<'T>) =
         observers.Add observer
         { new IDisposable with
            member this.Dispose() =
               lock sync <| remove observer
         }

do  let s = Subject()
    use d = s.Subscribe(fun x -> sprintf "%d" x |> Console.WriteLine)
    [1..12] |> Seq.iter s.Next



ReplaySubject
The ReplaySubject<T> type implements both IObserver<T> and IObservable<T>. 
It is functionally equivalent to the class of the same name in the Reactive Extensions (Rx) library 
with a replay buffer of a specified size .

open System
open System.Collections.Generic

type CircularBuffer<'T> (bufferSize:int) =
    let buffer = Array.zeroCreate<'T> bufferSize
    let mutable index = 0
    let mutable total = 0
    member this.Add value =
        if bufferSize > 0 then
            buffer.[index] <- value
            index <- (index + 1) % bufferSize
            total <- min (total + 1) bufferSize
    member this.Iter f =     
        let start = if total = bufferSize then index else 0
        for i = 0 to total - 1 do 
            buffer.[(start + i) % bufferSize] |> f                 

type message<'T> =
    | Add of IObserver<'T>
    | Remove of IObserver<'T>
    | Next of 'T
    | Completed
    | Error of exn

let startAgent (bufferSize:int) =
    let subscribers = LinkedList<_>()
    let buffer = CircularBuffer bufferSize               
    MailboxProcessor.Start(fun inbox ->
        let rec loop () = async {
            let! message = inbox.Receive()
            match message with
            | Add observer ->                    
                subscribers.AddLast observer |> ignore
                buffer.Iter observer.OnNext
                return! loop ()
            | Remove observer ->
                subscribers.Remove observer |> ignore
                return! loop ()
            | Next value ->                                       
                for subscriber in subscribers do
                    subscriber.OnNext value
                buffer.Add value
                return! loop () 
            | Error e ->
                for subscriber in subscribers do
                    subscriber.OnError e
            | Completed ->
                for subscriber in subscribers do
                    subscriber.OnCompleted ()
        }
        loop ()
    )

type ReplaySubject<'T> (bufferSize:int) =
    let bufferSize = max 0 bufferSize
    let agent = startAgent bufferSize    
    let subscribe observer =
        observer |> Add |> agent.Post
        { new System.IDisposable with
            member this.Dispose () =
                observer |> Remove |> agent.Post
        }
    member this.Next value = Next value |> agent.Post
    member this.Error error = Error error |> agent.Post
    member this.Completed () = Completed |> agent.Post    
  
    interface System.IObserver<'T> with
        member this.OnNext value = Next value |> agent.Post
        member this.OnError error = Error error |> agent.Post
        member this.OnCompleted () = Completed |> agent.Post
  
    member this.Subscribe(observer:System.IObserver<'T>) =
        subscribe observer
  
    interface System.IObservable<'T> with
        member this.Subscribe observer = subscribe observer                   

and Subject<'T>() = inherit ReplaySubject<'T>(0)


do  let subject = ReplaySubject(3)            
    use d = subject.Subscribe(fun (x:int) -> System.Console.WriteLine x)
    subject.Next(10)
    subject.Next(11)
    use d = subject.Subscribe(fun (x:int) -> System.Console.WriteLine x)
    System.Console.ReadLine() |> ignore 





  module Observable =
        open System
        open System.Collections.Generic

        module Subject =
            /// Subject state maintained inside of the mailbox loop
            module State =
                type t<'T> = {
                    observers : IObserver<'T> list
                    stopped   : bool
                }

                let empty() = {observers=[]; stopped=false}

            /// Messages required for the mailbox loop
            module Message =
                type t<'T> =
                | Add       of IObserver<'T>
                | Remove    of IObserver<'T>
                | Next      of 'T
                | Error     of exn
                | Completed

            /// Type t that implements IObservable<'T> and IObserver<'T>
            type t<'T>() =

                let error() = raise(new System.InvalidOperationException("Subject already completed"))

                let mbox = MailboxProcessor<Message.t<'T>>.Start(fun inbox ->
                    let rec loop(t:State.t<'T>) = async {
                        let! req = inbox.Receive()

                        match req with
                        | Message.Add(observer) ->
                            if not(t.stopped) then
                                return! loop ({t with observers = t.observers @ [observer]})
                            else error()

                        | Message.Remove(observer) ->
                            if not(t.stopped) then
                                let t = {t with observers = t.observers |> List.filter(fun f -> f <> observer)}
                                return! loop t
                            else error()

                        | Message.Next(valuJe) ->
                            if not(t.stopped) then
                                t.observers |> List.iter(fun o -> o.OnNext(value))
                                return! loop t
                            else error()

                        | Message.Error(err) ->
                            if not(t.stopped) then
                                t.observers |> List.iter(fun o -> o.OnError(err))
                                return! loop t
                            else error()

                        | Message.Completed ->
                            if not(t.stopped) then
                                t.observers |> List.iter(fun o -> o.OnCompleted())
                                let t = {t with stopped = true}
                                return! loop t
                            else error()
                    }

                    loop (State.empty())
                )

                /// Raises OnNext in all the observers
                member x.Next value  = Message.Next(value)  |> mbox.Post
                /// Raises OnError in all the observers
                member x.Error ex    = Message.Error(ex)    |> mbox.Post
                /// Raises OnCompleted in all the observers
                member x.Completed() = Message.Completed    |> mbox.Post

                interface IObserver<'T> with
                    member x.OnNext value   = x.Next(value)
                    member x.OnError ex     = x.Error(ex)
                    member x.OnCompleted()  = x.Completed()

                interface IObservable<'T> with
                    member x.Subscribe(observer:IObserver<'T>) =
                        observer |> Message.Add |> mbox.Post
                        { new IDisposable with
                            member x.Dispose() =
                                observer |> Message.Remove |> mbox.Post }
///////

module Observable

open System
open System.Collections.Generic

type Subject<'T> () =
   let sync = obj()
   let mutable stopped = false
   let observers = List<IObserver<'T>>()
   let iter f = observers |> Seq.iter f
   let onCompleted () =
      if not stopped then
         stopped <- true
         iter (fun observer -> observer.OnCompleted())
   let onError ex () =
      if not stopped then
         stopped <- true
         iter (fun observer -> observer.OnError(ex))
   let next value () =
      if not stopped then
         iter (fun observer -> observer.OnNext(value))
   let remove observer () =
      observers.Remove observer |> ignore
   member x.Next value = lock sync <| next value
   member x.Error ex = lock sync <| onError ex
   member x.Completed () = lock sync <| onCompleted
   interface IObserver<'T> with
      member x.OnCompleted() = x.Completed()
      member x.OnError ex = x.Error ex
      member x.OnNext value = x.Next value
   interface IObservable<'T> with
      member this.Subscribe(observer:IObserver<'T>) =
         observers.Add observer
         { new IDisposable with
            member this.Dispose() =
               lock sync <| remove observer
         }

do  let s = Subject()
    use d = s.Subscribe(fun x -> sprintf "%d" x |> Console.WriteLine)
    [1..12] |> Seq.iter s.Next            


type RequestGate(n : int) =
    let semaphore = new Semaphore(initialCount = n, maximumCount = n)
    member x.AsyncAcquire(?timeout) =
        async {let! ok = Async.AwaitWaitHandle(semaphore,
                                               ?millisecondsTimeout = timeout)
               if ok then
                   return
                     {new System.IDisposable with
                         member x.Dispose() =
                             semaphore.Release() |> ignore}
               else
                   return! failwith "couldn't acquire a semaphore" }


let using (ie : #System.IDisposable) f =
    try f(ie)
    finally ie.Dispose()
//val using : ie:'a -> f:('a -> 'b) -> 'b when 'a :> System.IDisposable



// ----------------------------------------------------------------------------

type private CircularBuffer<'T> (bufferSize:int) =
    let buffer = Array.zeroCreate<'T> bufferSize
    let mutable index = 0
    let mutable total = 0
    member this.Add value =
        if bufferSize > 0 then
            buffer.[index] <- value
            index <- (index + 1) % bufferSize
            total <- min (total + 1) bufferSize
    member this.Iter f =     
        let start = if total = bufferSize then index else 0
        for i = 0 to total - 1 do 
            buffer.[(start + i) % bufferSize] |> f

type private BufferAgentMessage<'T> =
    | Add of IObserver<'T>
    | Remove of IObserver<'T>
    | Next of 'T
    | Completed
    | Error of exn

module private BufferAgent =
    let start (bufferSize:int) =
        let subscribers = LinkedList<_>()
        let buffer = CircularBuffer bufferSize
        MailboxProcessor.Start(fun inbox ->
            let rec loop () = async {
                let! message = inbox.Receive()
                match message with
                | Add observer ->                   
                    subscribers.AddLast observer |> ignore
                    buffer.Iter observer.OnNext
                    return! loop ()
                | Remove observer ->
                    subscribers.Remove observer |> ignore
                    return! loop ()
                | Next value ->
                    for subscriber in subscribers do
                        subscriber.OnNext value
                    buffer.Add value
                    return! loop ()
                | Error e ->
                    for subscriber in subscribers do
                        subscriber.OnError e
                | Completed ->
                    for subscriber in subscribers do
                        subscriber.OnCompleted ()
            }
            loop ()
        )

[<Interface>]
type ISubject<'TIn,'TOut> =
    inherit System.IObserver<'TIn>
    inherit System.IObservable<'TOut>

type ReplaySubject<'T> (bufferSize:int) =
    let bufferSize = max 0 bufferSize
    let agent = BufferAgent.start bufferSize    
    let subscribe observer =
        observer |> Add |> agent.Post
        { new System.IDisposable with
            member this.Dispose () =
                observer |> Remove |> agent.Post
        }

    member this.OnNext value = Next value |> agent.Post
    member this.OnError error = Error error |> agent.Post
    member this.OnCompleted () = Completed |> agent.Post    
    member this.Subscribe(observer:System.IObserver<'T>) = subscribe observer

    interface ISubject<'T,'T> with
        member this.OnNext value = Next value |> agent.Post
        member this.OnError error = Error error |> agent.Post
        member this.OnCompleted () = Completed |> agent.Post
        member this.Subscribe observer = subscribe observer

and Subject<'T>() = inherit ReplaySubject<'T>(0)










module Async =
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
                    

