namespace TestingFSharp

open System
open NUnit.Framework
open FsUnit

module Translator =
    let ShouldFizz(input) = input % 3 = 0
    let ShouldBuzz(input) = input % 5 = 0

    let TranslateExtractMethod(input) = 
        if ShouldFizz input && ShouldBuzz input then "FizzBuzz"
        elif ShouldFizz input then "Fizz"
        elif ShouldBuzz input then "Buzz"
        else input.ToString()

type FizzBuzzTranslator() =
    static member Translate(input) =
        if input % 3 = 0 && input % 5 = 0 then "FizzBuzz"
        elif input % 3 = 0 then "Fizz"
        elif input % 5 = 0 then "Buzz"
        else input.ToString()

    static member TranslatePrivateMethods(input) =
        let ShouldFizz(input) = input % 3 = 0
        let ShouldBuzz(input) = input % 5 = 0
        if ShouldFizz input && ShouldBuzz input then "FizzBuzz"
        elif ShouldFizz input then "Fizz"
        elif ShouldBuzz input then "Buzz"
        else input.ToString()

    static member TranslateBuild(input) =
        let Fizzy x s = if x % 3 = 0 then s + "Fizz" else s
        let Buzzy x s = if x % 5 = 0 then s + "Buzz" else s
        let Other x s = if String.IsNullOrEmpty s then x.ToString() else s
        Fizzy input "" |> Buzzy input |> Other input

    static member TranslateBuildPartialFunction(input) =
        let Multiples divisor result x s = if x % divisor = 0 then s + result else s
        let Fizzy = Multiples 3 "Fizz"
        let Buzzy = Multiples 5 "Buzz"
        let Other x s = if String.IsNullOrEmpty s then x.ToString() else s
        Fizzy input "" |> Buzzy input |> Other input

    static member TranslatePatternMatchingWhen(input) =
        match input with
        | _ when input % 15 = 0 -> "FizzBuzz"
        | _ when input % 3 = 0 -> "Fizz"
        | _ when input % 5 = 0 -> "Buzz"
        | _ -> input.ToString()

    static member TranslatePatternMatchingNumber(input) =
        let fizzy = input % 3
        let buzzy = input % 5
        match fizzy, buzzy with
        | 0 , 0 -> "FizzBuzz"
        | 0 , _ -> "Fizz"
        | _ , 0 -> "Buzz"
        | _ , _ -> input.ToString()

    static member TranslatePatternMatchingBool(input) =
        let fizzy = input % 3 = 0
        let buzzy = input % 5 = 0
        match fizzy, buzzy with
        | true , true -> "FizzBuzz"
        | true , false -> "Fizz"
        | false , true -> "Buzz"
        | false , false -> input.ToString()

    static member TranslatePatternMatchingActive(input) =
        let (| FizzBuzzy | Fizzy | Buzzy | None |) x = 
            if x % 3 = 0 && x % 5 = 0 then FizzBuzzy
            elif x % 3 = 0 then Fizzy
            elif x % 5 = 0 then Buzzy
            else None
        match input with
        | FizzBuzzy -> "FizzBuzz"
        | Fizzy -> "Fizz"
        | Buzzy -> "Buzz"
        | None -> input.ToString()

[<TestFixture>] 
type ``FizzBuzz Tests`` ()=
    [<TestCase(1, "1")>]
    [<TestCase(2, "2")>]
    [<TestCase(3, "Fizz")>]
    [<TestCase(5, "Buzz")>]
    [<TestCase(6, "Fizz")>]
    [<TestCase(10, "Buzz")>]
    [<TestCase(15, "FizzBuzz")>]
    member x.
        ``input is expected`` (input : int, expected : string)= 
                FizzBuzzTranslator.Translate(input) |> should equal expected
                Translator.TranslateExtractMethod(input) |> should equal expected
                FizzBuzzTranslator.TranslatePrivateMethods(input) |> should equal expected
                FizzBuzzTranslator.TranslateBuild(input) |> should equal expected
                FizzBuzzTranslator.TranslateBuildPartialFunction(input) |> should equal expected
                FizzBuzzTranslator.TranslatePatternMatchingWhen(input) |> should equal expected
                FizzBuzzTranslator.TranslatePatternMatchingNumber(input) |> should equal expected
                FizzBuzzTranslator.TranslatePatternMatchingBool(input) |> should equal expected
                FizzBuzzTranslator.TranslatePatternMatchingActive(input) |> should equal expected

    [<Test>]
    member x.
        ``execute`` ()=
            let result = {1 .. 100} |> Seq.map(fun n -> FizzBuzzTranslator.Translate(n))
            printfn "%A " (Seq.toList result)