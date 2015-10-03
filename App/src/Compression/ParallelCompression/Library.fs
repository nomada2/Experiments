namespace ParallelCompression
 
open System
open System.IO
open System.IO.Compression
open System.Collections.Generic
 
module Compression =
 
    type Stream with
        member x.WriteSize32(size:int) =
            let bytes = BitConverter.GetBytes(size)
            x.AsyncWrite(bytes, 0, bytes.Length)
 
        member x.WriteSize64(size:int64) =
            let bytes = BitConverter.GetBytes(size)
            x.AsyncWrite(bytes, 0, bytes.Length)
 
        member x.ReadSizeInt() = async {
            let size = Array.zeroCreate<byte> (sizeof<int>)
            let! bytesRead = x.AsyncRead(size, 0, size.Length)
            return BitConverter.ToInt32(size, 0) }
 
 
        member x.ReadSizeInt64() = async {
            let size = Array.zeroCreate<byte> (sizeof<int64>)
            let! bytesRead = x.AsyncRead(size, 0, size.Length)
            return BitConverter.ToInt64(size, 0) }
 
 
    // Todo - size chunk
    // size threads ?? from quick sort
    // stream opened as async
    // get bytes data from agnets object and Memeory Stream ??
    // fast array Jon Papas
    // benchmarking inton CSV and chart
 
    type ParallelCompression() =
 
        let compress bytes = async {
            use memStream = new MemoryStream()
            use gzipStreamm = new GZipStream(memStream, CompressionMode.Compress)
            do! gzipStreamm.AsyncWrite(bytes, 0, bytes.Length)
            gzipStreamm.Flush()
            gzipStreamm.Close()
            gzipStreamm.Dispose()
            memStream.Flush()
            return memStream.GetBuffer() }
           
 
        member x.Compress (inputStream:Stream) (outputStream:Stream) = async {
            //let _ = inputStream.Seek(0L, SeekOrigin.Begin)
            let sourceLength = inputStream.Length
 
            // Write total size to destination
            do! outputStream.WriteSize64(sourceLength) 
 
            // ToDo calculate this
            let chunkSize = 1048576L // 1 MB
            try
                let rec startCompression streamLength (chunkSize:int64) = async {
                    match streamLength with                  
                    | n when n > 0L ->
                            let! bytesRead = inputStream.AsyncRead(int chunkSize)
                       
                            let! compressedBytes= compress bytesRead
                            
                            // write out the chunck size
                            do! outputStream.WriteSize64(chunkSize)
                            // write out the compressedData size
                            do! outputStream.WriteSize32(compressedBytes.Length)
 
                            // write out the compressed chunk
                            do! outputStream.AsyncWrite(compressedBytes, 0, compressedBytes.Length)
                       
                            let sourceLength = streamLength - chunkSize
                            let chunkSize = Math.Min(chunkSize, sourceLength)
                            return! startCompression sourceLength chunkSize 
                    | _ -> () }

                do! startCompression sourceLength chunkSize
            finally
                    inputStream.Dispose()
                    outputStream.Dispose()                                                   
            }

        member x.Decompress (inputStream:Stream) (outputStream:Stream) = async {
 
            let! sourceLength = inputStream.ReadSizeInt64()
           
            try
                let rec startDecompression streamLength = async {
                    match streamLength with
                    | 0L -> ()
                    | n ->  let! chunkSize = inputStream.ReadSizeInt64()
                            if (chunkSize > streamLength) then
                                raise (InvalidDataException())
                            let! storedSize = inputStream.ReadSizeInt()
                            let! compressedData = inputStream.AsyncRead(storedSize)
                          
                            use memStream = new MemoryStream(compressedData)
                            use gzipStream = new GZipStream(memStream, CompressionMode.Decompress)                      
                            let! uncompressedData = gzipStream.AsyncRead(int chunkSize)
                           
                            do! outputStream.AsyncWrite(uncompressedData, 0, uncompressedData.Length) 
 
                            return! startDecompression (streamLength - chunkSize) }
           
                do! startDecompression sourceLength
            finally
                inputStream.Dispose()
                outputStream.Dispose() }
 
        static member CompressTask (inputStream:Stream) (outputStream:Stream)  =
            let p = ParallelCompression()
            p.Compress inputStream outputStream |> Async.StartAsTask
 
        static member DecompressTask (inputStream:Stream) (outputStream:Stream)  =
            let p = ParallelCompression()
            p.Decompress inputStream outputStream |> Async.StartAsTask
 
 
 
//////////////////////////
 
type IODetailsCompress = {Bytes:byte array; Index:int; ChunkSize:byte array; CompressedDataSize:byte array option;  IsLastChunk:bool}
type IODetailsDecompress = {Bytes:byte array; Index:int; ChunkSize:int64; IsLastChunk:bool}
 

type ParalleCompression() =
         
            static member Compress (inputStream:Stream, outputStream:Stream
                                   ,?continutaion:(Stream * Stream -> unit)
                                   ,?chunkSize:int64
                                   ,?numberWorkers:int
                                   ,?cancelTokenSource:System.Threading.CancellationTokenSource) =
                
                if (not inputStream.CanRead) then raise(Exception("Input Stream read exception"))
                if (not outputStream.CanWrite) then raise(Exception("Output Stream write exception"))
                
                inputStream.Seek(0L, SeekOrigin.Begin) |> ignore
                outputStream.Seek(0L, SeekOrigin.Begin) |> ignore
                
                let continutaion = defaultArg continutaion (fun _ -> ())                
                let chunkSize = match chunkSize with
                                | None ->  Math.Min(inputStream.Length, 1048576L)
                                | Some(c) -> Math.Min(inputStream.Length, c)

                let numberWorkers = let numberOfChunks = ((inputStream.Length / chunkSize) |> int) 
                                    match numberWorkers with
                                    | Some(w) -> Math.Min(numberOfChunks, w)
                                    | None -> Math.Min(numberOfChunks, System.Environment.ProcessorCount)
                let cancelTokenSource = defaultArg cancelTokenSource (new System.Threading.CancellationTokenSource())
                let cancelToken = cancelTokenSource.Token

                let rec processBuffer lastProcessedBuffer (muliplexState:IODetailsCompress list) = async {
                    match muliplexState with
                    | item::tail when item.Index = (lastProcessedBuffer + 1) ->
                                do! outputStream.AsyncWrite(item.ChunkSize, 0, item.ChunkSize.Length)
                                do! outputStream.AsyncWrite(item.CompressedDataSize.Value, 0, item.CompressedDataSize.Value.Length)
                                do! outputStream.AsyncWrite(item.Bytes, 0, item.Bytes.Length)
                                if item.IsLastChunk = true then
                                    return None
                                else
                                    return! processBuffer (lastProcessedBuffer + 1) muliplexState.Tail
                    | _ -> return Some(lastProcessedBuffer, muliplexState) }

                let multiplexAgent = MailboxProcessor.Start((fun inbox ->
                    let rec loop lastProcessedBuffer (muliplexState:IODetailsCompress list) = async {
                            let! msgIODetails = inbox.Receive()
                            let {Bytes=bytes; Index=index; ChunkSize=chunkSize; CompressedDataSize = dataSize } = msgIODetails
                            let muliplexState = msgIODetails::muliplexState                           
                            let muliplexState' = muliplexState |> List.sortBy(fun k -> k.Index)
                            let! processedBuffer = processBuffer lastProcessedBuffer muliplexState'
                            match processedBuffer with
                            | None ->   cancelTokenSource.Cancel()
                                        continutaion(inputStream, outputStream)                                
                            | Some(lpb, ms) -> return! loop lpb ms }
                    loop 0 []), cancelToken)
 
                let compressAgent(mapToAgent:MailboxProcessor<IODetailsCompress>) =
                   (fun (inbox:MailboxProcessor<IODetailsCompress>) ->
                        let rec loop() = async {
                            let! msgIODetails = inbox.Receive()                           
                            use memStream = new MemoryStream()
                            use gzipStream = new GZipStream(memStream, CompressionMode.Compress)
                            do! gzipStream.AsyncWrite(msgIODetails.Bytes, 0, msgIODetails.Bytes.Length)
                            gzipStream.Close()
                            let compressedData = memStream.GetBuffer()
                            let compressedDataSize = BitConverter.GetBytes(compressedData.Length)
                            mapToAgent.Post {msgIODetails with Bytes = compressedData; CompressedDataSize = Some(compressedDataSize)}
                            return! loop()  }
                        loop() )

                let agents = Array.init numberWorkers (fun i -> MailboxProcessor.Start(compressAgent(multiplexAgent), cancelToken))

                let rec sendChunks indexMessage agentIndex chunkSize sourceLength = async {
                    match sourceLength with                    
                    | n when n > 0L ->  
                            let! data = inputStream.AsyncRead(int chunkSize)
                            let sourceLength = sourceLength - chunkSize
                            agents.[agentIndex].Post   {   Bytes = data 
                                                           ChunkSize = BitConverter.GetBytes(chunkSize:int64)
                                                           CompressedDataSize = None
                                                           Index = indexMessage
                                                           IsLastChunk = sourceLength = 0L }
                            return! sendChunks (indexMessage + 1) ((agentIndex + 1) % numberWorkers) (Math.Min(chunkSize, sourceLength)) sourceLength
                    | _ -> ()  }

                let compress = 
                    async { let sourceLen = inputStream.Length
                            let size = BitConverter.GetBytes sourceLen
                            do! outputStream.AsyncWrite(size,0,size.Length)
                            do! sendChunks 1 0  chunkSize sourceLen }
                
                ignore <| cancelToken.Register(fun() -> agents |> Array.map(fun a -> (a :> IDisposable)) |> Array.iter(fun a -> a.Dispose()))
                Async.Start(compress, cancelToken)
                cancelToken
 


            static member Decompress (inputStream:Stream, outputStream:Stream
                                   ,?continutaion:(Stream * Stream -> unit)
                                   ,?numberWorkers:int
                                   ,?cancelTokenSource:System.Threading.CancellationTokenSource) =
                
                if (not inputStream.CanRead) then raise(Exception("Input Stream read exception"))
                if (not outputStream.CanWrite) then raise(Exception("Output Stream write exception"))
                
                inputStream.Seek(0L, SeekOrigin.Begin) |> ignore
                outputStream.Seek(0L, SeekOrigin.Begin) |> ignore
                
                let continutaion = defaultArg continutaion (fun _ -> ())                
                let numberWorkers = defaultArg numberWorkers System.Environment.ProcessorCount
                let cancelTokenSource = defaultArg cancelTokenSource (new System.Threading.CancellationTokenSource())
                let cancelToken = cancelTokenSource.Token

                let rec processBuffer lastProcessedBuffer (muliplexState:IODetailsDecompress list) = async {
                    match muliplexState with
                    | item::tail when item.Index = (lastProcessedBuffer + 1) ->
                                do! outputStream.AsyncWrite(item.Bytes, 0, item.Bytes.Length)
                                if item.IsLastChunk = true then
                                    return None
                                else
                                    return! processBuffer (lastProcessedBuffer + 1) muliplexState.Tail
                    | _ -> return Some(lastProcessedBuffer, muliplexState)}

                let multiplexAgent = MailboxProcessor.Start((fun inbox ->
                    let rec loop lastProcessedBuffer (muliplexState:IODetailsDecompress list) = async {
                        let! msgIODetails = inbox.Receive()
                        let {IODetailsDecompress.Bytes=data; Index=index; ChunkSize=chunkSize; IsLastChunk=isLastChunk} = msgIODetails
                        let muliplexState = msgIODetails::muliplexState       
                        let muliplexState' = muliplexState |> List.sortBy(fun k -> k.Index)
                        let! processedBuffer = processBuffer lastProcessedBuffer muliplexState'
                        match processedBuffer with
                        | None -> cancelTokenSource.Cancel()
                                  continutaion(inputStream, outputStream)                               
                        | Some(lpb, ms) -> return! loop lpb ms }
                    loop 0 []), cancelToken)
                           
                let decompressAgent (mapToAgent:MailboxProcessor<IODetailsDecompress>)=
                   (fun (inbox:MailboxProcessor<IODetailsDecompress>) ->
                        let rec loop() = async {
                            let! msgIODetails = inbox.Receive()   
                            let {IODetailsDecompress.Bytes=data; Index=index; ChunkSize=chunkSize; IsLastChunk=isLastChunk} = msgIODetails
                            use memStream = new MemoryStream(data)
                            use gzipStream = new GZipStream(memStream, CompressionMode.Decompress)
                            let! compressedData = gzipStream.AsyncRead(int chunkSize)
                            gzipStream.Close()
                            mapToAgent.Post {msgIODetails with Bytes = compressedData }
                            return! loop()  }
                        loop() )

                let agents = Array.init numberWorkers (fun i -> MailboxProcessor.Start(decompressAgent(multiplexAgent), cancelToken))

                let rec sendChunks indexMessage agentIndex sourceLength = async {
                    match sourceLength with                    
                    | n when n > 0L ->  
                            let! chunkSizeBytes = inputStream.AsyncRead(sizeof<int64>)
                            let chunkSize = BitConverter.ToInt64(chunkSizeBytes,0)
                            let! compressedSizeBytes = inputStream.AsyncRead(sizeof<int>)
                            let compressedSize = BitConverter.ToInt32(compressedSizeBytes, 0)
                            let! data = inputStream.AsyncRead(compressedSize)
                            agents.[agentIndex].Post   {   Bytes = data 
                                                           ChunkSize = chunkSize
                                                           Index = indexMessage
                                                           IsLastChunk = (sourceLength - chunkSize) = 0L }
                            
                            return! sendChunks (indexMessage + 1) ((agentIndex + 1) % numberWorkers) (sourceLength - chunkSize)
                    | _ -> ()  }

                let decompress = 
                    async { let! size = inputStream.AsyncRead(sizeof<int64>)                            
                            let sourceLen = BitConverter.ToInt64(size, 0)
                            outputStream.SetLength(sourceLen)
                            do! sendChunks 1 0  sourceLen }
                ignore <| cancelToken.Register(fun () -> agents |> Array.map (fun a -> (a :> IDisposable)) |> Array.iter(fun a -> a.Dispose()))
                Async.Start(decompress, cancelToken)
                cancelToken
