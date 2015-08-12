open System
open System.Collections.Generic
open System
open System.Reactive.Linq
open System.Reactive
open Nessos.Streams
open Nessos.LinqOptimizer
open Nessos.LinqOptimizer.FSharp

[<AutoOpenAttribute>]
module Stream = 
    type Stream<'T> = ('T -> unit) -> unit

    let inline ofArray (source: 'T[]) : Stream<'T> =
       fun k ->
          let mutable i = 0
          while i < source.Length do
                k source.[i]
                i <- i + 1          

    let inline filter (predicate: 'T -> bool) (stream: Stream<'T>) : Stream<'T> =
       fun k -> stream (fun value -> if predicate value then k value)

    let inline map (mapF: 'T -> 'U) (stream: Stream<'T>) : Stream<'U> =
       fun k -> stream (fun v -> k (mapF v))

    let inline iter (iterF: 'T -> unit) (stream: Stream<'T>) : unit =
       stream (fun v -> iterF v)

    let inline toArray (stream: Stream<'T>) : 'T [] =
       let acc = new List<'T>()
       stream |> iter (fun v -> acc.Add(v))
       acc.ToArray()

    let inline fold (foldF:'State->'T->'State) (state:'State) (stream:Stream<'T>) =
       let acc = ref state
       stream (fun v -> acc := foldF !acc v)
       !acc

    let inline reduce (reducer: ^T -> ^T -> ^T) (stream: Stream< ^T >) : ^T
          when ^T : (static member Zero : ^T) =
       fold (fun s v -> reducer s v) LanguagePrimitives.GenericZero stream

    let inline sum (stream : Stream< ^T>) : ^T
          when ^T : (static member Zero : ^T)
          and ^T : (static member (+) : ^T * ^T -> ^T) =
       fold (+) LanguagePrimitives.GenericZero stream

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
 
[<EntryPoint>]
let main argv = 
    let data = [|1L..1000000L|]
    let nums = [|1..10000000|]

    let seqValue = 
       data
       |> Seq.filter (fun x -> x%2L = 0L)
       |> Seq.map (fun x -> x * x)
       |> Seq.sum

    let streamValue =
       data
       |> Stream.ofArray
       |> Stream.filter (fun x -> x%2L = 0L)
       |> Stream.map (fun x -> x * x)
       |> Stream.sum


    let arrayValue =
       data
       |> Array.filter (fun x -> x%2L = 0L)
       |> Array.map (fun x -> x * x)
       |> Array.sum

    let streamValue =
       data
       |> Stream.ofArray
       |> Stream.filter (fun x -> x%2L = 0L)
       |> Stream.map (fun x -> x * x)
       |> Stream.sum


    // Linq Optimizer

    let queryLinqOptmizier =
        nums
        |> Query.ofSeq
        |> Query.filter (fun num -> num % 2 = 0)
        |> Query.map (fun num -> num * num)
        |> Query.sum


    printfn "Result: %d" <| Query.run queryLinqOptmizier // compile and execute

    let obsValue =
       data
       |> Observable.ofSeq
       |> Observable.filter (fun x -> x%2L = 0L)
       |> Observable.map (fun x -> x * x)
       |> Observable.sum
       |> Observable.first
 
    let streamValue =
       data
       |> Stream.ofArray
       |> Stream.filter (fun x -> x%2L = 0L)
       |> Stream.map (fun x -> x * x)
       |> Stream.sum
 
    0 // return an integer exit code
