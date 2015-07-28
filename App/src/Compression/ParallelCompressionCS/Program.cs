using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ParallelCompressionCS
{
    class Program
    {
        static void Main(string[] args)
        {
            var sw = new System.Diagnostics.Stopwatch();

            var fileName = @"c:\Temp\fsharp_template.csv";
            var fileName2 = @"c:\Temp\fsharp_template2.csv";
           
            //var fileName = @"c:\Temp\video.mp4";
            //var fileName2 = @"c:\Temp\video2.mp4";

            File.Delete(fileName2);


            //var asyncCopy = Comp.CopyStreamsAsyncParallel(File.OpenRead(fileName), File.OpenWrite(fileName2));

            //Task.WaitAll(asyncCopy);

            //var bytes1 = File.ReadAllBytes(fileName);
            //var bytes2 = File.ReadAllBytes(fileName2);

            //var areEqual = bytes1.SequenceEqual(bytes2);
            //Console.WriteLine(areEqual);

            var fileComp = @"C:\temp\comp.zip";
            var fileComp2 = @"C:\temp\comp2.zip";

            //sw.Start();
            //Comp.Compress(File.OpenRead(fileName), File.OpenWrite(fileComp));
            //Console.WriteLine("Total Compression Async {0}", sw.ElapsedMilliseconds);

            File.Delete(fileName2);
            File.Delete(fileComp);
            File.Delete(fileComp2);

            //var bytes = Comp.Compress(File.ReadAllBytes(fileName));
            //var deBytes = Comp.DeCompress(bytes);
            //File.WriteAllBytes(fileName2, deBytes);

            //bytes1 = File.ReadAllBytes(fileName);
            //bytes2 = File.ReadAllBytes(fileName2);

            //areEqual = bytes1.SequenceEqual(bytes2);
            //Console.WriteLine(areEqual);

            //sw.Reset();
            //sw.Start();
            //CompUtility.Compress(File.OpenRead(fileComp), File.OpenWrite(fileName2));
            //Console.WriteLine("Total Compression DataFlow {0}", sw.ElapsedMilliseconds);


            sw.Reset();
            sw.Start();
            CompressionChunks.CompressChunk(File.OpenRead(fileName), File.OpenWrite(fileComp));
            Console.WriteLine("Total Compression chunk {0}", sw.ElapsedMilliseconds);

           
            sw.Reset();
            sw.Start();
            CompressionChunks.DecompressChunk(File.OpenRead(fileComp), File.OpenWrite(fileName2));
            Console.WriteLine("Total Decompression chunk {0}", sw.ElapsedMilliseconds);


           var  bytes1 = File.ReadAllBytes(fileName);
            var bytes2 = File.ReadAllBytes(fileName2);

            var areEqual = bytes1.SequenceEqual(bytes2);
            Console.WriteLine(areEqual);


            File.Delete(fileName2);
            File.Delete(fileComp);
            File.Delete(fileComp2);

            sw.Reset();
            sw.Start();
            ParallelCompression.Compression.ParallelCompression.CompressTask(File.OpenRead(fileName), File.OpenWrite(fileComp)).Wait();
            Console.WriteLine("Total Compression F# chunk {0}", sw.ElapsedMilliseconds);

            sw.Reset();
            sw.Start();
            ParallelCompression.Compression.ParallelCompression.DecompressTask(File.OpenRead(fileComp), File.OpenWrite(fileName2)).Wait();
            Console.WriteLine("Total Decompression F# chunk {0}", sw.ElapsedMilliseconds);


            bytes1 = File.ReadAllBytes(fileName);
            bytes2 = File.ReadAllBytes(fileName2);

            areEqual = bytes1.SequenceEqual(bytes2);
            Console.WriteLine(areEqual);


            File.Delete(fileName2);
            File.Delete(fileComp);
            File.Delete(fileComp2);

            sw.Reset();
            sw.Start();
            CompressionDataFlow.Compress(File.OpenRead(fileName), File.OpenWrite(fileComp)).Wait();
            Console.WriteLine("Total DataFlow Compression chunk {0}", sw.ElapsedMilliseconds);
                        
            sw.Reset();
            sw.Start();
            DecompressionDataFlow.DecompressFastDataFlow(File.OpenRead(fileComp), File.OpenWrite(fileName2)).Wait();
            Console.WriteLine("Total DataFlow Decompression chunk {0}", sw.ElapsedMilliseconds);

            bytes1 = File.ReadAllBytes(fileName);
            bytes2 = File.ReadAllBytes(fileName2);

            areEqual = bytes1.SequenceEqual(bytes2);
            Console.WriteLine(areEqual);

            Console.WriteLine("OK Decomp {0}", CompareFIles(fileName2, fileName));

            Console.ReadLine();
        }

        private static byte[] GetFileHash(string file)
        {
            using (var stream = new FileStream(file, FileMode.Open, FileAccess.Read))
            {
                var hash = System.Security.Cryptography.HMACSHA256.Create();
                return hash.ComputeHash(stream);
            }
        }


        private static bool CompareFIles(string file1, string file2)
        {
            bool areEqual = true;
            using (var stream1 = new FileStream(file1, FileMode.Open, FileAccess.Read))
            using (var stream2 = new FileStream(file2, FileMode.Open, FileAccess.Read))
            {
                if (stream1.Length != stream2.Length)
                    areEqual = false;

                for (int i = 0; i < stream2.Length; i++)
                {
                    var s1 = stream1.ReadByte();
                    var s2 = stream2.ReadByte();
                    if (s1 != s2)
                    {
                        areEqual = false;
                    }
                }

                return areEqual;
            }
        }

    }
}
