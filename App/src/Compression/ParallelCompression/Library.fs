namespace ParallelCompression

open System
open System.IO
open System.IO.Compression

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
                    | 0L ->  return ()
                    | n ->  let! bytesRead = inputStream.AsyncRead(int chunkSize)
                        
                            let! compressedBytes= compress bytesRead
                            
                            // write out the chunck size
                            do! outputStream.WriteSize64(chunkSize)
                            // write out the compressedData size
                            do! outputStream.WriteSize32(compressedBytes.Length)

                            // write out the compressed chunk
                            do! outputStream.AsyncWrite(compressedBytes, 0, compressedBytes.Length)
                        
                            let sourceLength = streamLength - chunkSize
                            let chunkSize = Math.Min(chunkSize, sourceLength)
                            return! startCompression sourceLength chunkSize }
            
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