
type EmailAddress = EmailAddress of string

let CreateEmailAddressWithContinuations success failure (s:string) = 
    if System.Text.RegularExpressions.Regex.IsMatch(s,@"^\S+@\S+\.\S+$") 
        then success (EmailAddress s)
        else failure "Email address must contain an @ sign"

        // setup the functions 
let success (EmailAddress s) = printfn "success creating email %s" s        
let failure  msg = printfn "error creating email: %s" msg
let createEmail = CreateEmailAddressWithContinuations success failure

// test
let goodEmail = createEmail "x@example.com"
let badEmail = createEmail "example.com"

////////
type MaybeBuilder() =

    member this.Bind(x, f) = 
        match x with
        | None -> None
        | Some a -> f a

    member this.Return(x) = 
        Some x
   
let maybe = new MaybeBuilder()

let divideBy x y = x / y

let divideByWorkflow init x y z = 
    maybe 
        {
        let! a = init |> divideBy x
        let! b = a |> divideBy y
        let! c = b |> divideBy z
        return c
        }   

///////
type LoggingBuilder() =
    let log p = printfn "expression is %A" p

    member this.Bind(x, f) = 
        log x
        f x

    member this.Return(x) = 
        x

let logger = LoggingBuilder()

let loggedWorkflow = 
    logger
        {
        let! x = 42
        let! y = 43
        let! z = x + y
        return z
        }       
////////
type TraceBuilder() =
    member this.Bind(m, f) = 
        match m with 
        | None -> 
            printfn "Binding with None. Exiting."
        | Some a -> 
            printfn "Binding with Some(%A). Continuing" a
        Option.bind f m

    member this.Return(x) = 
        printfn "Returning a unwrapped %A as an option" x
        Some x

    member this.ReturnFrom(m) = 
        printfn "Returning an option (%A) directly" m
        m

    member this.Zero() = 
        printfn "Zero"
        None

    member this.Yield(x) = 
        printfn "Yield an unwrapped %A as an option" x
        Some x

    member this.YieldFrom(m) = 
        printfn "Yield an option (%A) directly" m
        m

    member this.Delay(f) = 
        printfn "Delay"
        f()

    member this.Delay(funcToDelay) = 
        let delayed = fun () ->
            printfn "%A - Starting Delayed Fn." funcToDelay
            let delayedResult = funcToDelay()
            printfn "%A - Finished Delayed Fn. Result is %A" funcToDelay delayedResult
            delayedResult  // return the result 

        printfn "%A - Delaying using %A" funcToDelay delayed
        delayed // return the new function        
        
    member this.Combine (a,b) = 
            match a,b with
            | Some a', Some b' ->
                printfn "combining %A and %A" a' b' 
                Some (a' + b')
            | Some a', None ->
                printfn "combining %A with None" a' 
                Some a'
            | None, Some b' ->
                printfn "combining None with %A" b' 
                Some b'
            | None, None ->
                printfn "combining None with None"
                None


    member this.Run(funcToRun) = 
        printfn "%A - Run Start." funcToRun
        let runResult = funcToRun()
        printfn "%A - Run End. Result is %A" funcToRun runResult
        runResult // return the result of running the delayed function


// make an instance of the workflow                
let trace = new TraceBuilder()

trace { 
    yield 1
    yield 2
    } |> printfn "Result for yield then yield: %A" 

type Internal = Internal of int option
type Delayed = Delayed of (unit -> Internal)

type TraceBuilder() =
    member this.Bind(m, f) = 
        match m with 
        | None -> 
            printfn "Binding with None. Exiting."
        | Some a -> 
            printfn "Binding with Some(%A). Continuing" a
        Option.bind f m

    member this.Return(x) = 
        printfn "Returning a unwrapped %A as an option" x
        Internal (Some x) 

    member this.ReturnFrom(m) = 
        printfn "Returning an option (%A) directly" m
        Internal m

    member this.Zero() = 
        printfn "Zero"
        Internal None

    member this.Combine (Internal x, Delayed g) : Internal = 
        printfn "Combine. Starting %A" g
        let (Internal y) = g()
        printfn "Combine. Finished %A. Result is %A" g y
        let o = 
            match x,y with
            | Some a, Some b ->
                printfn "Combining %A and %A" a b 
                Some (a + b)
            | Some a, None ->
                printfn "combining %A with None" a 
                Some a
            | None, Some b ->
                printfn "combining None with %A" b 
                Some b
            | None, None ->
                printfn "combining None with None"
                None
        // return the new value wrapped in a Internal
        Internal o                

    member this.Delay(funcToDelay) = 
        let delayed = fun () ->
            printfn "%A - Starting Delayed Fn." funcToDelay
            let delayedResult = funcToDelay()
            printfn "%A - Finished Delayed Fn. Result is %A" funcToDelay delayedResult
            delayedResult  // return the result 

        printfn "%A - Delaying using %A" funcToDelay delayed
        Delayed delayed // return the new function wrapped in a Delay

    member this.Run(Delayed funcToRun) = 
        printfn "%A - Run Start." funcToRun
        let (Internal runResult) = funcToRun()
        printfn "%A - Run End. Result is %A" funcToRun runResult
        runResult // return the result of running the delayed function

    member this.While(guard, body) =
        printfn "While: test"
        if not (guard()) 
        then 
            printfn "While: zero"
            this.Zero() 
        else
            printfn "While: body"
            this.Bind( body(), fun () -> 
                this.While(guard, body))  

member this.TryWith(body, handler) =
    try 
        printfn "TryWith Body"
        this.ReturnFrom(body())
    with 
        e ->
            printfn "TryWith Exception handling"
            handler e

member this.TryFinally(body, compensation) =
    try 
        printfn "TryFinally Body"
        this.ReturnFrom(body())
    finally 
        printfn "TryFinally compensation"
        compensation() 

member this.Using(disposable:#System.IDisposable, body) =
    let body2 = fun () -> body disposable
    this.TryFinally(body2, fun () -> 
        match disposable with 
            | null -> () 
            | disp -> disp.Dispose())

// make an instance of the workflow                
let trace = new TraceBuilder()

//////////
type TraceBuilder() =

    member this.Bind(m, f) = 
        Option.bind f m

    member this.Return(x) = Some x

    member this.ReturnFrom(x) = x

    member this.Yield(x) = Some x

    member this.YieldFrom(x) = x
    
    member this.Zero() = this.Return ()

    member this.Delay(f) = f

    member this.Run(f) = f()

    member this.While(guard, body) =
        if not (guard()) 
        then this.Zero() 
        else this.Bind( body(), fun () -> 
            this.While(guard, body))  

    member this.TryWith(body, handler) =
        try this.ReturnFrom(body())
        with e -> handler e

    member this.TryFinally(body, compensation) =
        try this.ReturnFrom(body())
        finally compensation() 

    member this.Using(disposable:#System.IDisposable, body) =
        let body2 = fun () -> body disposable
        this.TryFinally(body2, fun () -> 
            match disposable with 
                | null -> () 
                | disp -> disp.Dispose())

    member this.For(sequence:seq<_>, body) =
        this.Using(sequence.GetEnumerator(),fun enum -> 
            this.While(enum.MoveNext, 
                this.Delay(fun () -> body enum.Current)))
/////////                

type ListBuilder() =
    member this.Bind(m, f) = 
        m |> List.collect f

    member this.Zero() = 
        printfn "Zero"
        []
        
    member this.Yield(x) = 
        printfn "Yield an unwrapped %A as a list" x
        [x]

    member this.YieldFrom(m) = 
        printfn "Yield a list (%A) directly" m
        m

    member this.For(m,f) =
        printfn "For %A" m
        this.Bind(m,f)
        
    member this.Combine (a,b) = 
        printfn "combining %A and %A" a b 
        List.concat [a;b]

    member this.Delay(f) = 
        printfn "Delay"
        f()


// make an instance of the workflow                
let listbuilder = new ListBuilder()


///////////  

let DoSomething counter = 
    printfn "DoSomething. Counter=%i " counter
    counter + 1

let FinalResult counter = 
    printfn "FinalResult. Counter=%i " counter

let counterWorkflow = 
    let counter = 0 
    let counter' = DoSomething counter 
    let counter'' = DoSomething counter' 
    let counter''' = DoSomething counter'' 
    do FinalResult counter'''

/// -> transformed

type State<'a, 's> = State of ('s -> 'a * 's)

let runState (State s) a = s a
let getState = State (fun s -> (s,s))
let putState s = State (fun _ -> ((),s))

type StateBuilder() =
    member this.Return(a) = 
        State (fun s -> (a,s))
    member this.Bind(m,k) =
        State (fun s -> 
            let (a,s') = runState m s 
            runState (k a) s')
    member this.ReturnFrom (m) = m

let state' = new StateBuilder()



let DoSomething counter = 
    printfn "DoSomething. Counter=%i " counter
    counter + 1

let FinalResult counter = 
    printfn "FinalResult. Counter=%i " counter
    counter

// convert old functions to "state-aware" functions
let lift f = state {
    let! s = getState 
    return! putState (f s)
    }

// new functions
let DoSomething' = lift DoSomething
let FinalResult' = lift FinalResult

let counterWorkflow = 
    let s = state {
        do! DoSomething'
        do! DoSomething'
        do! DoSomething'
        do! FinalResult'
        } 
    runState s 0
