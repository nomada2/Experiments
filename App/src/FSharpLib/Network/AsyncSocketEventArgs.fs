﻿namespace Easj360FSharp

open System
open System.Net
open System.Net.Sockets

module AsyncSocketEventArgs =

/// An F#-friendly SocketAsyncEventArgs subclass.
    type AsyncSocketEventArgs(callback: SocketAsyncEventArgs -> unit) as x =
        inherit SocketAsyncEventArgs()
        let mutable disposed = false
        let subscription = x.Completed.Subscribe(callback)
        member private x.Dispose(disposing) =
            if not disposed then
                if disposing then
                    subscription.Dispose()
                base.Dispose()
                disposed <- true
        member x.CallbackSync() = callback x
        /// Call Close when using AsyncSocketEventArgs and not Dispose,
        /// or cast to IDisposable to use the explicit implementation.
        /// This is necessary as the dispose method is sealed on the
        /// SocketAsyncEventArgs
        member x.Close() =
            x.Dispose(true)
        override x.Finalize() =
            x.Dispose(false)
        interface IDisposable with
            member x.Dispose() =
                x.Dispose(true)
                GC.SuppressFinalize(x)
    // [/snippet]

    // [snippet: Socket extensions using the AsyncSocketEventArgs type]
    /// Helper method to make Async calls easier.  InvokeAsyncMethod ensures the callback always
    /// gets called even if an error occurs or the Async method completes synchronously.
    let inline invokeAsyncMethod(asyncmethod, args:AsyncSocketEventArgs) =
        let result = asyncmethod args
        if result <> true then
            args.CallbackSync()

    type Socket with 
        member s.AcceptAsync(args) =
            invokeAsyncMethod(s.AcceptAsync, args) 
        member s.ReceiveAsync(args) =
            invokeAsyncMethod(s.ReceiveAsync, args) 
        member s.SendAsync(args) =
            invokeAsyncMethod(s.SendAsync, args) 
        member s.ConnectAsync(args) =
            invokeAsyncMethod(s.ConnectAsync, args)
        member s.DisconnectAsync(args) =
            invokeAsyncMethod(s.DisconnectAsync, args)