// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 


    let arr = [1..10]

    let fold = arr
               |> List.fold(fun s t -> 
                                printfn "state %d - item %d" s t
                                s + t) 0


    let foldL = List.foldBack(fun s t -> 
                                    printfn "item %d - state %d" s t
                                    s + t) arr 0


    let text = System.IO.File.ReadAllText(@"c:\temp\fsharp_2013-2014.csv")
    let maxlen = 
        //text
        System.Text.RegularExpressions.Regex.Split(text, @"\W+")
        |> Array.toList
        |> List.map (fun s -> s.Length)
        |> List.reduce max



    0 // return an integer exit code
