using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.IO.Compression;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;

namespace ParallelCompressionCS
{
    public class CompressionDataFlow
    {
        struct CompressionDetails
        {
            public byte[] Bytes { get; set; }
            public int Sequence { get; set; }
            public byte[] ChunkSize { get; set; }
            public byte[] CompressedDataSize { get; set; }
            public bool IsProcessed { get; set; }
        }

        private static ConcurrentDictionary<int, CompressionDetails> multiplexState = new ConcurrentDictionary<int, CompressionDetails>();
        private static int lastProcessedBuffer = 0;
        private static readonly int MaxDegreeOfParallelism = 4;
        private static readonly int BoundedCapacity = 150;


        public static async Task Compress(Stream inputStream, Stream outputStream)
        {
            var buffer = new BufferBlock<CompressionDetails>(new DataflowBlockOptions { BoundedCapacity = BoundedCapacity });
            var compressorOptions = new ExecutionDataflowBlockOptions
            {
                MaxDegreeOfParallelism = MaxDegreeOfParallelism,
                BoundedCapacity = BoundedCapacity
            };

            var writerOptions = new ExecutionDataflowBlockOptions
            {
                BoundedCapacity = BoundedCapacity,
                SingleProducerConstrained = true
            };

            var compressor = new TransformBlock<CompressionDetails, CompressionDetails>(compressionDetails => Compress(compressionDetails), compressorOptions);
            var writer = new ActionBlock<CompressionDetails>(compressionDetailsWithSize => Multiplex(outputStream, compressionDetailsWithSize), writerOptions);

            buffer.LinkTo(compressor);
            compressor.LinkTo(writer);
            
            buffer.Completion.ContinueWith(task => compressor.Complete()); 
            compressor.Completion.ContinueWith(task => writer.Complete());

            long sourceLength = inputStream.Length;
            // Write total size to destination
            byte[] size = BitConverter.GetBytes(sourceLength);
            await outputStream.WriteAsync(size, 0, size.Length);

            long chunkSize = 1048576; // 1 MB
            int index = 0;
            while (sourceLength > 0)
            {
                byte[] data = new byte[chunkSize];
                int readCount = await inputStream.ReadAsync(data, 0, data.Length);

                byte[] bytes = new byte[readCount];
                Buffer.BlockCopy(data, 0, bytes, 0, readCount);

                CompressionDetails compressionDetails = new CompressionDetails
                {
                    Bytes = bytes,
                    ChunkSize = BitConverter.GetBytes(chunkSize),
                    Sequence = ++index
                };

                while (await buffer.SendAsync(compressionDetails) != true) { }

                sourceLength -= chunkSize;
                if (sourceLength < chunkSize)
                    chunkSize = sourceLength;

                if (sourceLength == 0)
                    buffer.Complete();
            }
            writer.Completion.Wait();

            await outputStream.FlushAsync();
            inputStream.Dispose();
            outputStream.Dispose();
        }

        private static async Task<CompressionDetails> Multiplex(Stream outputStream, CompressionDetails compressionDetailsWithSize)
        {
            CompressionDetails v = compressionDetailsWithSize;
            multiplexState.AddOrUpdate(v.Sequence, v, (i, s) => s);

            var orderedState = multiplexState.Where(k => !k.Value.IsProcessed).OrderBy(kv => kv.Key);

            int index = lastProcessedBuffer;
            foreach (var item in orderedState)
            {
                CompressionDetails c = item.Value;
                if (c.Sequence == (lastProcessedBuffer + 1))
                {
                    await outputStream.WriteAsync(c.ChunkSize, 0, c.ChunkSize.Length);
                    await outputStream.WriteAsync(c.CompressedDataSize, 0, c.CompressedDataSize.Length);
                    await outputStream.WriteAsync(c.Bytes, 0, c.Bytes.Length);

                    lastProcessedBuffer = c.Sequence;
                    c.IsProcessed = true;
                    multiplexState[c.Sequence] = c;
                }
                else
                    break;
            }
            return v;
        }

        private static async Task<CompressionDetails> Compress(CompressionDetails compressionDetails)
        {
            byte[] data = compressionDetails.Bytes;
            using (MemoryStream compressedDataStream = new MemoryStream())
            {
                using (GZipStream streamCompressed = new GZipStream(compressedDataStream, CompressionMode.Compress))
                {
                    // write chunk in the compressed stream
                    await streamCompressed.WriteAsync(data, 0, data.Length);
                }
                byte[] compressedData = compressedDataStream.GetBuffer();
                byte[] compressedDatasize = BitConverter.GetBytes(compressedData.Length);

                CompressionDetails v = new CompressionDetails
                {
                    Bytes = compressedData,
                    ChunkSize = compressionDetails.ChunkSize,
                    Sequence = compressionDetails.Sequence,
                    CompressedDataSize = compressedDatasize
                };
                return v;
            }
        }
    }
}