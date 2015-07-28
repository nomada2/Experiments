#I ".\\bin\\Debug"
#r ".\\bin\\Debug\\Hopac.dll"
#r ".\\bin\\Debug\\Hopac.Core.dll"
#r ".\\bin\\Debug\\Hopac.Platform.dll"

open Hopac
open Hopac.Job.Infixes
open System.Threading.Tasks
open System
 
let reduceParallelTasks<'a> f (ie: 'a array) = 
    let rec reduceRec f (ie: 'a array) = 
        match ie.Length with
        | 1 -> ie.[0]
        | 2 -> f ie.[0] ie.[1]
        | len -> 
            let h = len / 2
            let o1 = Task.Run(fun _ -> reduceRec f ie.[0..h - 1])
            let o2 = Task.Run(fun _ -> reduceRec f ie.[h..])
            f o1.Result o2.Result
    match ie.Length with
    | 0 -> failwith "Sequence contains no elements"
    | _ -> reduceRec f ie
 
let reduceParallelAsync<'a> f (ie: 'a array) = 
    let rec reduceRec f (ie: 'a array) = 
        async { 
            match ie.Length with
            | 1 -> return ie.[0]
            | 2 -> return f ie.[0] ie.[1]
            | len -> 
                let h = len / 2
                let! o1a = Async.StartChild <| reduceRec f ie.[0..h - 1]
                let! o2 = reduceRec f ie.[h..]
                let! o1 = o1a
                return f o1 o2
        }
    match ie.Length with
    | 0 -> failwith "Sequence contains no elements"
    | _ -> Async.RunSynchronously <| reduceRec f ie
 
let reduceParallelHopac<'a> f (a: 'a array) = 
    let rec reduceRec (f, ie: 'a array) = 
        match ie.Length with
        | 1 -> Job.result ie.[0]
        | 2 -> Job.result (f ie.[0] ie.[1])
        | len -> 
            let h = len / 2
            reduceRec (f, ie.[0..h - 1]) <*> Job.delayWith reduceRec (f, ie.[h..]) |>> fun (x, y) -> f x y
    match a.Length with
    | 0 -> failwith "Sequence contains no elements"
    | _ -> run <| reduceRec (f, a)
 
let cleanup() =
  for _ in 1..5 do
    GC.Collect ()
    Threading.Thread.Sleep 50
     
let a = [| 1L..5000000L |]
 
Array.reduce (+) a
cleanup()
reduceParallelTasks (+) a
cleanup()
reduceParallelHopac (+) a
cleanup()
reduceParallelAsync (+) a

reduceParallelHopac (fun c -> fun x -> Timer.Global.timeOutMillis 30;x+c) a