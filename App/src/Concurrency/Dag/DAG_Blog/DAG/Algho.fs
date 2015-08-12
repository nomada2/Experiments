namespace DAG

open System
open System.Collections.Generic

type DirectGraph(value:int) =

    let mutable e = 0

    let vertices  = Array.init value (fun _ -> List<int>())

    member this.Vertex = value
    member this.Edge = e

    member this.AddEdge(v:int, w:int) =
           vertices.[v].Add(w)
           e <- e + 1

    
    member this.getVertex(v:int) =
        vertices.[v]

    member this.reverse() =
        let r = DirectGraph(value)
        for i = 0 to value - 1 do
            for w in vertices.[i] do
                r.AddEdge(w, i)
        r
    



//let dg = Digraph(10)
//dg.V
//dg.E
//
//dg.addEdge(4,6)
//
//dg.getAdj(8)
//dg.getAdj(4)
//
//dg.reverse()