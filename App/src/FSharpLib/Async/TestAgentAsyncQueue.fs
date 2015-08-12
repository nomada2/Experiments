﻿namespace Easj360FSharp

open System
open System.Collections.Generic
open AgentHelper

//type Agent<'T> = MailboxProcessor<'T>

/// Type of messages internally used by 'BlockingQueueAgent<T>'
type internal BlockingAgentMessage<'T> = 
  // Send item to the queue and block until it is added 
  | Add of 'T * AsyncReplyChannel<unit> 
  // Get item from the queue (block if no item available)
  | Get of AsyncReplyChannel<'T>


/// Agent that implements an asynchronous blocking queue
type BlockingQueueAgent<'T>(maxLength) =
  // We keep the number of elements in the queue in a local field to
  // make it immediately available (so that 'Count' property doesn't 
  // have to use messages - which would be a bit slower)
  [<VolatileField>]
  let mutable count = 0

  let agent = Agent.Start(fun agent ->
    // List of items and list of pending 'Put' calls
    let queue = new Queue<_>()
    let pending = new Queue<_>()

    // If the queue is empty, we cannot handle 'Get' message
    let rec emptyQueue() = 
      agent.Scan(fun msg ->
        match msg with 
        | Add(value, reply) -> Some <| async {
            queue.Enqueue(value)
            count <- queue.Count
            reply.Reply()
            return! nonEmptyQueue() }
        | _ -> None )

    // If the queue is non-empty, we handle all messages
    and nonEmptyQueue() = async {
      let! msg = agent.Receive()
      match msg with 
      | Add(value, reply) -> 
          // If the queue has space, we add item and notif caller back;
          // if it is full, we enqueue the request (and block caller)
          if queue.Count < maxLength then 
            queue.Enqueue(value)
            count <- queue.Count
            reply.Reply()
          else 
            pending.Enqueue(value, reply) 
          return! nonEmptyQueue()
      | Get(reply) -> 
          let item = queue.Dequeue()
          // We took item from the queue - check if there are any blocked callers
          // and values that were not added to the queue because it was full
          while queue.Count < maxLength && pending.Count > 0 do
            let itm, caller = pending.Dequeue()
            queue.Enqueue(itm)
            caller.Reply()
          count <- queue.Count
          reply.Reply(item)
          // If the queue is empty then switch the state, otherwise loop
          if queue.Count = 0 then return! emptyQueue()
          else return! nonEmptyQueue() }

    // Start with an empty queue
    emptyQueue() )


  /// Returns the number of items in the queue (immediately)
  /// (excluding items that are being added by callers that have been
  /// blocked because the queue was full)
  member x.Count = count

  /// Asynchronously adds item to the queue. The operation ends when
  /// there is a place for the item. If the queue is full, the operation
  /// will block until some items are removed.
  member x.AsyncAdd(v:'T, ?timeout) = 
    agent.PostAndAsyncReply((fun ch -> Add(v, ch)), ?timeout=timeout)

  /// Asynchronously gets item from the queue. If there are no items
  /// in the queue, the operation will block unitl items are added.
  member x.AsyncGet(?timeout) = 
    agent.PostAndAsyncReply(Get, ?timeout=timeout)




type MultiAgent() =
    
    let queueAgent = new BlockingQueueAgent<int>(32)

    let parallelWorker n f = 
        MailboxProcessor.Start(fun inbox ->
            let workers = 
                Array.init n (fun i -> MailboxProcessor.Start(f))
            let rec loop i = async {
                let! msg = inbox.Receive()
                do! queueAgent.AsyncAdd(msg) 
                //workers.[i].Post(msg)
                return! loop ((i+1) % n)

            }
            loop 0
        )
  
    let agent = 
            parallelWorker 8 (fun inbox ->
                let rec loop() = async {                
                    let! msg = queueAgent.AsyncGet() //inbox.Receive()
                    do printfn "Received %d from %d" msg System.Threading.Thread.CurrentThread.ManagedThreadId
                    do! Async.Sleep(10)
                    return! loop()
                }
                loop()
            )

    member x.PostValue(v) =
        agent.Post(v)
        //do! queueAgent.AsyncAdd(msg) 
