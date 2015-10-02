open System



let inline stringf format (x : ^a) = 
    (^a : (member ToString : string -> string) (x, format))


DateTimeOffset.Now |> stringf "o";;
DateTimeOffset.Now |> stringf "T";;
DateTime.Now |> stringf "t";;
TimeSpan.FromDays 42. |> stringf "c";;
0.42m |> stringf "p0";;



open System.Collections.Generic

type Cache<'T> private () =
    static let d = Dictionary<string,'T>()

    static member Format(format: Printf.StringFormat<'T>) : 'T =
        let key = format.Value
        match d.TryGetValue(key) with
        | true, r -> r
        | _ ->
            let r = sprintf format
            d.Add(key, r)
            r

let sprintf' fmt =
    Cache<_>.Format(fmt)

#time

for i in 1 .. 10000 do
    ignore (sprintf "%i" 15)

for i in 1 .. 10000 do
    ignore (sprintf' "%i" 15)