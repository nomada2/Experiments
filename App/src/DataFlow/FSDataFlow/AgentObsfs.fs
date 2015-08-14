
module Observable 
        open System
        open System.Threading
        open System.Threading.Tasks
        open System.Collections.Generic
        open System.Reactive
        open System.Reactive.Subjects


        let applyTask(task:Task<'a>) : IObservable<'a> =
            let subject = new AsyncSubject<'a>()
            let continuation (t:Task<'a>) : unit = 
                match t.IsFaulted with
                | true -> subject.OnError(t.Exception)
                | _ ->  subject.OnNext(t.Result)
                        subject.OnCompleted()
            ignore <| task.ContinueWith continuation 
            subject :> IObservable<'a>


        let task = Task.Factory.StartNew(new Func<int>(fun () -> Thread.Sleep 1000; 8))
        (applyTask task).Subscribe({ new IObserver<int> with
                                    member x.OnNext(a) = printfn "Received %d" a
                                    member x.OnCompleted() = printfn "Completed"
                                    member x.OnError(ex) = () }) |> ignore


        let applyAsync(task:Async<'a>) : IObservable<'a> =
            let subject = new AsyncSubject<'a>()            
            Async.StartWithContinuations(task, (fun con -> subject.OnNext(con)
                                                           subject.OnCompleted()),
                                               (fun exn -> subject.OnError(exn)),
                                               (fun cnl -> ()))
            subject :> IObservable<'a>
        
        let operation = async{ do! Async.Sleep 1000; 
                               return 8 }

        (operation |> applyAsync).Subscribe(
                    { new IObserver<int> with
                            member x.OnNext(a) = printfn "Received %d" a
                            member x.OnCompleted() = printfn "Completed"
                            member x.OnError(ex) = () }) |> ignore

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