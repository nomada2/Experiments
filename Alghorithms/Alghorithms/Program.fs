
open System




let rec MergeSort (input: int list) =   
   // Merges 2 lists together in ascended sorted order.  
   let Merge (left: int list, right: int list) =  
     let rec mergeLists (left: int list, right: int list, output: int list) =  
       match (left, right, output) with  
         | ([], right, output) -> output@right  
         | (left, [], output) -> output@left  
         | (left, right, output) when left.Head < right.Head -> mergeLists (left.Tail, right, output@[left.Head])  
         | (left, right, output) ->mergeLists (left, right.Tail, output@[right.Head])  
     mergeLists (left, right, [])  
   // Process the input.  
   if input.Length = 0 then []  
   else if input.Length = 1 then input  
   else if input.Length = 2 then  
     if input.[0] > input.[1] then [input.[1]; input.[0]]  
     else input  
   else  
     // Valid list size found, sort and merge.  
     let left = MergeSort (input |> Seq.take (input.Length / 2) |> Seq.toList)  
     let right = MergeSort (input |> Seq.skip (input.Length / 2) |> Seq.toList)  
     Merge (left, right)  


[<EntryPoint>]
let main argv = 
    
    let rnd = Random(100)

    let lst = [ for x = 0 to 100 do yield rnd.Next() ]

    let sortedLst = MergeSort lst






    printfn "%A" argv
    0 // return an integer exit code
