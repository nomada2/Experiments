open System
open System.Reactive

module AgentStuff =
    type Agent<'T> = MailboxProcessor<'T>
    
    [<InterfaceAttribute>]
    type IAgentPost<'a, 'b> =
        abstract PostAction : msg:'a -> 'b
        //default PostAction

   // [<SealedAttribute>]
    type AgentAction<'a>(f: 'a -> unit) =
        
        let agentAction  = 
                 Agent<_>.Start(fun inbox ->
                        let rec loop n = async {
                            let! msg = inbox.Receive()
                            f msg
                            return! loop (n + 1)
                                }
                        loop 0 )
           

        member x.PostAction(msg:'a) =
                agentAction.Post(msg)

        member x.SendAction(msg:'a) =
                agentAction.Post(msg)

        interface IDisposable with
            member __.Dispose() =
                    (agentAction :> IDisposable).Dispose() 

    type AgentTransformer<'a,'b>(f:'a -> 'b) =
        
            let subject = new System.Reactive.Subjects.Subject<'b>()
        
            let agent = Agent<_>.Start(fun inbox ->
                    let rec loop observers = async {
                        let! msg = inbox.Receive()
                        match msg with
                        | ObserverAdd o -> return! loop (o::observers)
                        | ObserverRemove o -> return! loop (observers |> List.filter(fun f -> f <> o))
                        | PostAction(a,reply) -> 
                                let res:'b = f a
                                reply.Reply(res)
                                observers |> List.iter(fun o -> o.OnNext(res))
                                return! loop observers }
                    loop [] )

            member x.SendAction(msg:'a) =
                agent.PostAndAsyncReply(fun (reply:AsyncReplyChannel<'b>) -> PostAction(msg, reply))

            interface IAgentPost<'a, 'b> with
                member __.PostAction(msg:'a) =
                    agent.PostAndReply(fun (reply:AsyncReplyChannel<'b>) -> PostAction(msg, reply))

            interface IObservable<'b> with
                member x.Subscribe(observer:IObserver<'b>) =
                    observer |> ObserverAdd |> agent.Post
                    { new IDisposable with
                        member x.Dispose() =
                            observer |> ObserverRemove |> agent.Post }
    and private Message<'a, 'b> =
        | ObserverRemove of IObserver<'b>
        | ObserverAdd of IObserver<'b>
        | PostAction of 'a * AsyncReplyChannel<'b>

    type AgentTransformer<'a,'b> with
        member x.LinkTo() =
            ()

open AgentStuff
[<EntryPoint>]
let main argv = 

    let obs = Observable.Subject.t<string>()



    
    let a = new AgentAction<string>(fun s -> printfn "%s" s)
    a.PostAction("Ciao")

    let t = AgentTransformer<int, string>(fun s -> let r = s * s 
                                                   printfn "result %s" (string r)
                                                   string r)

    t.Subscribe(fun s -> printfn "Hello from Sub %s" s) |> ignore
                     
    //t.LinkTo()
                                                         
    //let res = t.
//
//    printf "%s" res

    Console.ReadKey() |> ignore
    0 // return an integer exit code

