module SocketEvt

open System.Net.Sockets

type A = System.Net.Sockets.SocketAsyncEventArgs
type B = System.ArraySegment<byte>

exception SocketIssue of SocketError with
    override this.ToString() =
        string this.Data0

/// Wraps the Socket.xxxAsync logic into F# async logic.
let inline asyncDo (op: A -> bool) (prepare: A -> unit)
    (select: A -> 'T) =
    Async.FromContinuations <| fun (ok, error, _) ->
        let args = new A()
        prepare args
        let k (args: A) =
            match args.SocketError with
            | System.Net.Sockets.SocketError.Success ->
                let result = select args
                args.Dispose()
                ok result
            | e ->
                args.Dispose()
                error (SocketIssue e)
        args.add_Completed(System.EventHandler<_>(fun _ -> k))
        if not (op args) then
            k args

/// Prepares the arguments by setting the buffer.
let inline setBuffer (buf: B) (args: A) =
    args.SetBuffer(buf.Array, buf.Offset, buf.Count)

let Accept (socket: Socket) =
    asyncDo socket.AcceptAsync ignore (fun a -> a.AcceptSocket)

let Receive (socket: Socket) (buf: B) =
    asyncDo socket.ReceiveAsync (setBuffer buf)
        (fun a -> a.BytesTransferred)

let Send (socket: Socket) (buf: B) =
    asyncDo socket.SendAsync (setBuffer buf) ignore
