module Program
open ParallelCompression
open System
open System.IO

[<EntryPointAttribute>]
let main args =

    let files = [   @"c:\data\fsharp_2013-2014.csv"
                    @"c:\temp\bugghina1.jpg"
                    @"c:\temp\bugghina5.jpg"
                    @"c:\temp\bugghina10.jpg"
                    @"c:\temp\bugghina4.jpg"
                ] 

    let files' = [for f in Directory.GetFiles(@"C:\Data\Shakespeare") -> f]

    for file in (files @ files') do

//        let nameZipFile = @"c:\TempZip\" + Path.GetFileNameWithoutExtension(file) + ".zip"
//        use inputStream = new FileStream(file, FileMode.Open, FileAccess.Read, FileShare.Read, 0x1000, true)
//        use outputStream = new FileStream(nameZipFile, FileMode.Create, FileAccess.Write, FileShare.None, 0x1000, true)
//        printfn "Analyzing %s..." (Path.GetFileNameWithoutExtension(file))        
//        let token = ParalleCompression.Compress(inputStream, outputStream, 
//                               continutaion=(fun (input, output) -> input.Dispose()
//                                                                    output.Dispose()
//                                                                    printfn "Completed Zipping %s..." (Path.GetFileNameWithoutExtension(file))
//                                                                    let nameUnZipFile = @"c:\TempUnZip\" + Path.GetFileNameWithoutExtension(nameZipFile) + Path.GetExtension(file)
//                                                                    let inputStream' = new FileStream(nameZipFile, FileMode.Open, FileAccess.Read, FileShare.Read, 0x1000, true)
//                                                                    let outputStream' = new FileStream(nameUnZipFile, FileMode.Create, FileAccess.Write, FileShare.None, 0x1000, true)
//                                                                    let t = ParallelCompression.Compression.ParallelCompression()
//                                                                    let d = t.Decompress inputStream' outputStream' 
//                                                                    d |> Async.RunSynchronously
//                                                                    inputStream'.Dispose()
//                                                                    outputStream'.Dispose()
//                                                                    printfn "Completed Unzipping %s..." (Path.GetFileNameWithoutExtension(file))
//                                                                    
//                                                                    System.Threading.Thread.Sleep(9000)
//                                                                    let bytes1 = File.ReadAllBytes(file)
//                                                                    let bytes2 = File.ReadAllBytes(nameUnZipFile)
//                                                                    if bytes1.Length = bytes2.Length then
//                                                                        for i = 0 to bytes1.Length - 1 do
//                                                                            if not(bytes1.[i] = bytes2.[i]) then 
//                                                                                raise (Exception("No Equal"))
//                                                                            if i % 1024 = 0 then
//                                                                                printfn "Comparising process succesfull so far for %s" (Path.GetFileNameWithoutExtension(file))
//                                                                        printfn "Comparising process succesfull for %s" (Path.GetFileNameWithoutExtension(file))
//                                                                    ()))

        let nameZipFile = @"c:\TempZip\" + Path.GetFileNameWithoutExtension(file) + ".zip"
        use inputStream = new FileStream(file, FileMode.Open, FileAccess.Read, FileShare.Read, 0x1000, true)
        use outputStream = new FileStream(nameZipFile, FileMode.Create, FileAccess.Write, FileShare.None, 0x1000, true)
        printfn "Analyzing %s..." (Path.GetFileNameWithoutExtension(file))        
        let token = ParalleCompression.Compress(inputStream, outputStream, 
                               continutaion=(fun (input, output) -> input.Dispose()
                                                                    output.Dispose()
                                                                    printfn "Completed Zipping %s..." (Path.GetFileNameWithoutExtension(file))
                                                                    let nameUnZipFile = @"c:\TempUnZip\" + Path.GetFileNameWithoutExtension(nameZipFile) + Path.GetExtension(file)
                                                                    let inputStream' = new FileStream(nameZipFile, FileMode.Open, FileAccess.Read, FileShare.Read, 0x1000, true)
                                                                    let outputStream' = new FileStream(nameUnZipFile, FileMode.Create, FileAccess.Write, FileShare.None, 0x1000, true)
                                                                    let t = ParalleCompression.Decompress(inputStream', outputStream', 
                                                                                        continutaion=(fun (input, output) ->    input.Dispose()
                                                                                                                                output.Dispose()
                                                                                                                                printfn "Completed Unzipping %s..." (Path.GetFileNameWithoutExtension(file))
                                                                    
                                                                                                                                System.Threading.Thread.Sleep(9000)
                                                                                                                                let bytes1 = File.ReadAllBytes(file)
                                                                                                                                let bytes2 = File.ReadAllBytes(nameUnZipFile)
                                                                                                                                if bytes1.Length = bytes2.Length then
                                                                                                                                    for i = 0 to bytes1.Length - 1 do
                                                                                                                                        if not(bytes1.[i] = bytes2.[i]) then
                                                                                                                                            raise (Exception("No Equal"))
                                                                                                                                        if i % 1024 = 0 then
                                                                                                                                            printfn "Comparising process succesfull so far for %s" (Path.GetFileNameWithoutExtension(file))
                                                                                                                                    printfn "Comparising process succesfull for %s" (Path.GetFileNameWithoutExtension(file))))
                                                                    ()))

        System.Threading.Thread.Sleep(16000)
        ()

    Console.ReadLine() |> ignore


    0

