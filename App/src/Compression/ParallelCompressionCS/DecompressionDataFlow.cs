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
    class DecompressionDataFlow
    {
        private static readonly int MaxDegreeOfParallelism = 4;
        private static readonly int BoundedCapacity = 150;

        struct DecompressionDetails
        {

            public byte[] Bytes { get; set; }
            public int Sequence { get; set; }
            public long ChunkSize { get; set; }
            //  public int StoredSize { get; set; }
            public bool IsProcessed { get; set; }
        }


        private static ConcurrentDictionary<int, DecompressionDetails> multiplexState = new ConcurrentDictionary<int, DecompressionDetails>();
        private static int lastProcessBuffer = 0;

        public static async Task DecompressFastDataFlow(Stream inputStream, Stream outputStream)
        {
            var buffer = new BufferBlock<DecompressionDetails>(new DataflowBlockOptions { BoundedCapacity = BoundedCapacity });
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

            var compressor = new TransformBlock<DecompressionDetails, DecompressionDetails>(compressionDetails => Decompress(compressionDetails), compressorOptions);
            var writer = new ActionBlock<DecompressionDetails>(compressionDetailsWithSize => Multiplex(buffer, outputStream, compressionDetailsWithSize), writerOptions);


            buffer.LinkTo(compressor);
            compressor.LinkTo(writer);

            buffer.Completion.ContinueWith(task => compressor.Complete());      
            compressor.Completion.ContinueWith(task => writer.Complete());


            byte[] size = new byte[sizeof(long)];
            await inputStream.ReadAsync(size, 0, size.Length);
            // convert the size to number
            long sourceLength = BitConverter.ToInt64(size, 0);

            int index = 0;
            while (sourceLength > 0)
            {
                size = new byte[sizeof(long)];
                await inputStream.ReadAsync(size, 0, size.Length);

                // convert the size back to number
                long chunkSize = BitConverter.ToInt64(size, 0);
                if (chunkSize > sourceLength) throw new InvalidDataException("");

                // read the compressed size
                size = new byte[sizeof(int)];
                await inputStream.ReadAsync(size, 0, size.Length);

                // convert the size back to number
                int storedSize = BitConverter.ToInt32(size, 0);

                byte[] compressedData = new byte[storedSize];
                int readCount = await inputStream.ReadAsync(compressedData, 0, compressedData.Length);

                DecompressionDetails decompressionDetails = new DecompressionDetails
                {
                    Bytes = compressedData,
                    ChunkSize = chunkSize,             
                    Sequence = ++index
                };

                while (await buffer.SendAsync(decompressionDetails) != true) { }

                sourceLength -= chunkSize;
                if (sourceLength < chunkSize)
                    chunkSize = sourceLength;

                if (sourceLength == 0)
                    buffer.Complete();
            }
            writer.Completion.Wait();

            outputStream.Flush();
            inputStream.Dispose();
            outputStream.Dispose();
        }

        private static async Task Multiplex(BufferBlock<DecompressionDetails> buffer, Stream outputStream, DecompressionDetails compressionDetails)
        {
            multiplexState.AddOrUpdate(compressionDetails.Sequence, compressionDetails, (i, s) => s);

            var orderedState = multiplexState.Where(k => !k.Value.IsProcessed).OrderBy(kv => kv.Key);

            int index = lastProcessBuffer;
            foreach (var item in orderedState)
            {
                DecompressionDetails c = item.Value;
                if (c.Sequence == (lastProcessBuffer + 1))
                {
                    await outputStream.WriteAsync(c.Bytes, 0, c.Bytes.Length);

                    lastProcessBuffer = c.Sequence;
                    c.IsProcessed = true;
                    multiplexState[c.Sequence] = c;
                }
                else
                    break;
            }
        }

        private static async Task<DecompressionDetails> Decompress(DecompressionDetails compressionDetails)
        {
            byte[] compressedData = new byte[compressionDetails.ChunkSize];

            // uncompressed the chunk
            using (MemoryStream uncompressedDataStream = new MemoryStream(compressionDetails.Bytes))
            using (GZipStream streamUncompressed = new GZipStream(uncompressedDataStream, CompressionMode.Decompress))
            {
                int index = 0;
                int count = compressedData.Length;
                while (index < compressedData.Length - 1)
                {
                    // read the chunk in the compressed stream
                    var bytesRead = await streamUncompressed.ReadAsync(compressedData, index, count);
                    index += bytesRead;
                    count -= bytesRead;
                }
            }

            compressionDetails = new DecompressionDetails
            {
                Bytes = compressedData,
                ChunkSize = compressionDetails.ChunkSize,
                Sequence = compressionDetails.Sequence,
                IsProcessed = compressionDetails.IsProcessed
            };
            return compressionDetails;
        }
    }
}
