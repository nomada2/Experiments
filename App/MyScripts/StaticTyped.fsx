
let inline speak (a: ^a) = 
    let x = (^a : (member speak: unit -> string) (a))
    printfn "It said: %s" x
    let y = (^a : (member talk: unit -> string) (a))
    printfn "Then it said %s" y

type duck() =
    member x.speak() = "quack"
    member x.talk() = "quackity quack"

type dog() =
    member x.speak() = "woof"
    member x.talk() = "arrrr"

let x = new duck()
let y = new dog()
speak x
speak y


