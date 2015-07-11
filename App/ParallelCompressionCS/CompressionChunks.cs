using System;
using System.Collections.Generic;
using System.IO;
using System.IO.Compression;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ParallelCompressionCS
{
  public  class CompressionChunks
    {
        public static void CompressChunk(Stream inputStream, Stream outputStream)
        {
            // choose compress type Deflate or GZip ??

            long sourceLength = inputStream.Length;
            // Write total size to destination
            byte[] size = BitConverter.GetBytes(sourceLength);
            outputStream.Write(size, 0, size.Length);

            long chunkSize = 1048576; // 1 MB

            try
            {
                while (sourceLength > 0)
                {
                    //// Write total size to destination
                    //outputStream.Write(size, 0, size.Length);

                    // read chunk
                    byte[] data = new byte[chunkSize];
                    inputStream.Read(data, 0, data.Length);

                    // compress chunk
                    using (MemoryStream compressedDataStream = new MemoryStream())
                    {
                        using (GZipStream streamCompressed = new GZipStream(compressedDataStream, CompressionMode.Compress))
                        {

                            // write chunk in the compressed stream
                            streamCompressed.Write(data, 0, data.Length);
                        }
                        byte[] compressedData = compressedDataStream.GetBuffer();

                        // write out the chunck size
                        size = BitConverter.GetBytes(chunkSize);
                        outputStream.Write(size, 0, size.Length);
                        //WriteSizeLong(outputStream, chunkSize);

                        // write out the compressed size
                        size = BitConverter.GetBytes(compressedData.Length);
                        outputStream.Write(size, 0, size.Length);
                        //WriteSizeInt(outputStream, compressedData.Length);

                        // write out the compressed chunk
                        outputStream.Write(compressedData, 0, compressedData.Length); // size ?

                        sourceLength -= chunkSize;
                        if (sourceLength < chunkSize)
                            chunkSize = sourceLength;
                    }

                }
            }
            finally
            {
                inputStream.Close();
                outputStream.Close();
            }
        }

        public static void DecompressChunk(Stream inputStream, Stream outputStream)
        {
            // choose compress type Deflate or GZip ??

            // read sourceLength size
            // read the chunk size
            byte[] size = new byte[sizeof(long)];
            inputStream.Read(size, 0, size.Length);

            // convert the size to number
            long sourceLength = BitConverter.ToInt64(size, 0);
            long chunkSize = 0;
            int storedSize = 0;

            try
            {
                while (sourceLength > 0)
                {
                    // read the chunk size
                    size = new byte[sizeof(long)];
                    inputStream.Read(size, 0, size.Length);

                    // convert the size back to number
                    chunkSize = BitConverter.ToInt64(size, 0);
                    if (chunkSize > sourceLength) throw new InvalidDataException("");

                    // read the compressed size
                    size = new byte[sizeof(int)];
                    inputStream.Read(size, 0, size.Length);

                    // convert the size back to number
                    storedSize = BitConverter.ToInt32(size, 0);
                    // if (storedSize > sourceLength) throw new InvalidDataException("");

                    byte[] uncompressedData = new byte[chunkSize];
                    byte[] compressedData = new byte[storedSize];
                    long position = inputStream.Position;
                    inputStream.Read(compressedData, 0, compressedData.Length);

                    // uncompressed the chunk
                    using (MemoryStream uncompressedDataStream = new MemoryStream(compressedData))
                    using (GZipStream streamUncompressed = new GZipStream(uncompressedDataStream, CompressionMode.Decompress))
                    {

                        // read the chunk in the compressed stream
                        streamUncompressed.Read(uncompressedData, 0, uncompressedData.Length);
                    }
                    // write the uncompressed chunk
                    outputStream.Write(uncompressedData, 0, uncompressedData.Length);

                    // subtruct the chunk size from the file size
                    sourceLength -= chunkSize;

                    if (sourceLength < chunkSize)
                        chunkSize = sourceLength;
                }
            }
            finally
            {
                inputStream.Close();
                outputStream.Close();
            }
        }

    }
}
