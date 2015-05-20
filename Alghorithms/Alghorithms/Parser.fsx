﻿
type ParserResult<'a> =
    | Success of 'a * list<char> 
    | Failure
    
type Parser<'a> = list<char> -> ParserResult<'a>
 
let CharParser c : Parser<char> =
    let p stream =
        match stream with
        | x::xs when x = c -> Success(x, xs)
        | _ -> Failure
    in p
 
let Either (p1: Parser<'a>) (p2: Parser<'a>) : Parser<'a> =
    let p stream =
        match p1 stream with
        | Failure -> p2 stream
        | res -> res 
    in p
 
let DigitParser =
    ['0'..'9']
    |> List.map CharParser
    |> List.reduce Either
 
let Apply (p: Parser<'a>) (f: 'a -> 'b) : Parser<'b> =
    let q stream =
        match p stream with
        | Success(x, rest) -> Success(f x, rest)
        | Failure -> Failure
    in q
 
let DigitParserInt = Apply DigitParser (fun c -> (int c) - (int '0'))
 
let rec Many (p: Parser<'a>) : Parser<List<'a>> =
    let q stream =
        match p stream with
        | Failure -> Success([], stream)
        | Success(x, rest) ->  (Apply (Many p) (fun xs -> x::xs)) rest
    in q
 
let IntegerParser : Parser<int> =
    Apply (Many DigitParserInt) (List.reduce (fun x y -> x * 10 + y))
 


printfn "%A" (IntegerParser ['1'; '2'; '3'; '4'])