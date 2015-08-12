using System;
using System.Collections.Generic;
using System.Collections.Concurrent;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Threading;
using System.Threading.Tasks.Dataflow;
using System.IO;
using System.IO.Compression;

namespace ParallelCompressionCS
{
    public class CompUtility
    {
        public static long CopyStreamToStream(Stream src, Stream dst)
        {
            long numCopied = 0;
            byte[] buffer = new byte[0x1000];
            int numRead;
            while ((numRead = src.Read(buffer, 0, buffer.Length)) > 0)
            {
                dst.Write(buffer, 0, numRead);
                numCopied += numRead;
            }
            return numCopied;
        }

        public static async Task<long> CopyStreamToStreamAsync(Stream src, Stream dst)
        {
            long numCopied = 0;
            byte[] buffer = new byte[0x1000];

            int numRead;
            while ((numRead = await src.ReadAsync(buffer, 0, buffer.Length)) > 0)
            {
                await dst.WriteAsync(buffer, 0, numRead);
                numCopied += numRead;
            }
            return numCopied;
        }

        public static async Task<long> CopyStreamsAsyncParallel(Stream src, Stream dst)
        {
            long numCopied = 0;
            byte[][] buffer = new byte[2][];
            buffer[0] = new byte[0x1000];
            buffer[1] = new byte[0x1000];

            var index = 0;
            int numRead = await src.ReadAsync(buffer[index], 0, buffer[index].Length);

            while (numRead > 0)
            {
                var writeAsync = dst.WriteAsync(buffer[index], 0, numRead);
                index = index ^= 1;
                var readAsync = src.ReadAsync(buffer[index], 0, buffer[index].Length);

                Task.WaitAll(writeAsync, readAsync);
                numRead = readAsync.Result;

                numCopied += numRead;
            }
            await dst.FlushAsync();
            src.Dispose();
            dst.Dispose();

            return numCopied;

        }
      
        private const int BufferSize = 256;

        public static byte[] Compress(byte[] bytes)
        {
            using (var compressedStream = new MemoryStream())
            using (var zipStream = new GZipStream(compressedStream, CompressionMode.Compress))
            {
                zipStream.Write(bytes, 0, bytes.Length);
                zipStream.Close();
                return compressedStream.ToArray();
            }
        }

        public static byte[] DeCompress(byte[] bytes)
        {
            using (var compressedStream = new MemoryStream(bytes))
            using (var zipStream = new GZipStream(compressedStream, CompressionMode.Decompress))
            using (var resultStream = new MemoryStream())
            {
                zipStream.CopyTo(resultStream);
                return resultStream.ToArray();
            }
        }

        public static void Compress(Stream inputStream, Stream outputStream)
        {
            var buffer = new BufferBlock<byte[]>();
            var compressor = new TransformBlock<byte[], byte[]>(bytes => Compress(bytes));
            var writer = new ActionBlock<byte[]>(bytes => outputStream.Write(bytes, 0, bytes.Length));

            buffer.LinkTo(compressor);
            buffer.Completion.ContinueWith(task => compressor.Complete());

            compressor.LinkTo(writer);
            compressor.Completion.ContinueWith(task => writer.Complete());

            var readBuffer = new byte[BufferSize];
            while (true)
            {
                int readCount = inputStream.Read(readBuffer, 0, BufferSize);
                if (readCount > 0)
                {
                    var bytes = new byte[readCount];
                    Buffer.BlockCopy(readBuffer, 0, bytes, 0, readCount);
                    while (!buffer.Post(bytes))
                    {
                    }
                }
                if (readCount != BufferSize)
                {
                    buffer.Complete();
                    break;
                }
            }
            writer.Completion.Wait();

            outputStream.Flush();
            inputStream.Dispose();
            outputStream.Dispose();
        }

        public static void DeCompress(Stream inputStream, Stream outputStream)
        {
            var buffer = new BufferBlock<byte[]>();
            var decompressor = new TransformBlock<byte[], byte[]>(bytes => DeCompress(bytes));
            var writer = new ActionBlock<byte[]>(bytes => outputStream.Write(bytes, 0, bytes.Length));

            buffer.LinkTo(decompressor);
            buffer.Completion.ContinueWith(task => decompressor.Complete());

            decompressor.LinkTo(writer);
            decompressor.Completion.ContinueWith(task => writer.Complete());

            var readBuffer = new byte[BufferSize];
            while (true)
            {
                int readCount = inputStream.Read(readBuffer, 0, BufferSize);
                if (readCount > 0)
                {
                    var bytes = new byte[readCount];
                    Buffer.BlockCopy(readBuffer, 0, bytes, 0, readCount);
                    while (!buffer.Post(bytes))
                    {
                    }
                }
                if (readCount != BufferSize)
                {
                    buffer.Complete();
                    break;
                }
            }
            writer.Completion.Wait();

            outputStream.Flush();
            inputStream.Dispose();
            outputStream.Dispose();
        }





    }
}
