open System



let inline stringf format (x : ^a) = 
    (^a : (member ToString : string -> string) (x, format))


DateTimeOffset.Now |> stringf "o";;
DateTimeOffset.Now |> stringf "T";;
DateTime.Now |> stringf "t";;
TimeSpan.FromDays 42. |> stringf "c";;
0.42m |> stringf "p0";;
