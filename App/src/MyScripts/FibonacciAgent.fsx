//#nowarn "40"

open System

/// Types (alias types)
type Agent<'a> = MailboxProcessor<'a>

/// Domain agents
let print fn =
  Agent.Start(fun inbox ->
    let rec loop = async {
      let! msg = inbox.Receive()
      fn msg
      return! loop }
    loop)

let duplicate a1 a2 fn =
  Agent.Start(fun inbox ->
    let rec loop = async {
      let! msg = inbox.Receive()
      fn a1 a2 msg
      return! loop }
    loop)

let delay a fn =
  Agent.Start(fun inbox ->
    let rec loop = async {
      let! msg = inbox.Receive()
      fn a msg
      return! loop }
    loop)

let add a fn =
  Agent.Start(fun inbox ->
    let rec loop state = async {
      let! msg = inbox.Receive()
      state |> function | None -> () | Some v -> fn a (0I+v+msg);
      return! loop (Some (msg))}
    loop (None))

/// Domain functions
let out = lazy print (fun msg -> printfn "%A" msg)
let rec delta2int =
  lazy
    duplicate out (pairsInt:Lazy<Agent<bigint>>)
      (fun a1 a2 msg -> a1.Value.Post msg; a2.Value.Post msg)
and prefixInt0 = lazy delay delta2int (fun a msg -> a.Value.Post msg)
and prefixInt1 = lazy delay prefixInt0 (fun a msg -> a.Value.Post msg)
and pairsInt = lazy add prefixInt1 (fun a msg -> a.Value.Post msg)

prefixInt0.Value.Post 0I
prefixInt1.Value.Post 1I

