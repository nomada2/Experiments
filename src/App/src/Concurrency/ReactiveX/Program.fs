open System
open System.Reactive
open System.Reactive.Linq
open System.Threading

let keyPress() =
    seq {   while true do
                let currentKey = Console.ReadKey(true)
                match currentKey.Key with
                | ConsoleKey.Enter -> yield None
                | c -> yield Some(c) }


[<EntryPoint>]
let main argv = 

    let nums = [0..10] |> List.toSeq

    let query = query { for num in nums do //.AsQueryExpr()
                        where (num % 2 = 0)
                        sumBy (num * num) }  



    
    let timeToStop = new ManualResetEvent(false)
    let keyPresses = keyPress().ToObservable()
//
//    keyPresses
//    |> Observable.filter(fun c -> c.IsSome)
//    |> Observable.GroupBy(fun k -> k.Value)
//    |> 
//



    0 // return an integer exit code

//
//    class GroupBy_Simple
//{
//    static IEnumerable<ConsoleKeyInfo> KeyPresses()
//    {
//        for (; ; )
//        {
//            var currentKey = Console.ReadKey(true);
//
//            if (currentKey.Key == ConsoleKey.Enter)
//                yield break;
//            else
//                yield return currentKey;
//        }
//    }
//    static void Main()
//    {
//        var timeToStop = new ManualResetEvent(false);
//        var keyPresses = KeyPresses().ToObservable();
//
//        var groupedKeyPresses =
//            from k in keyPresses
//            group k by k.Key into keyPressGroup
//            select keyPressGroup;
//
//        Console.WriteLine("Press Enter to stop.  Now bang that keyboard!");
//
//        groupedKeyPresses.Subscribe(keyPressGroup =>
//        {
//            int numberPresses = 0;
//
//            keyPressGroup.Subscribe(keyPress =>
//            {
//                Console.WriteLine(
//                    "You pressed the {0} key {1} time(s)!",
//                    keyPress.Key,
//                    ++numberPresses);
//            },
//            () => timeToStop.Set());
//        });
//
//        timeToStop.WaitOne();
//    }
//}