using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Collections.Generic;
using System.Threading.Tasks;
using System.Collections.Concurrent;
using System.Text;
using System.Threading;
using System.Collections;


namespace MapReduceWords
{
    public class WordReducer
    {
        public static ConcurrentBag<string> wordBag = new ConcurrentBag<string>();
        public BlockingCollection<string> wordChunks = new BlockingCollection<string>(wordBag);

        /// 

        /// 1.  Produce 250 character or less chunks of text.
        /// 2.  Break chunks on the first space encountered before 250 characters.
        /// 

        /// 
        /// 
        public IEnumerable produceWordBlocks(string fileText)
        {
            int blockSize = 250;
            int startPos = 0;
            int len = 0;

            for (int i = 0; i < fileText.Length; i++)
            {
                i = i + blockSize > fileText.Length - 1 ? fileText.Length - 1 : i + blockSize;

                while (i >= startPos && fileText[i] != ' ')
                {
                    i--;
                }

                if (i == startPos)
                {
                    i = i + blockSize > (fileText.Length - 1) ? fileText.Length - 1 : i + blockSize;
                    len = (i - startPos) + 1;
                }
                else
                {
                    len = i - startPos;
                }

                yield return fileText.Substring(startPos, len).Trim();
                startPos = i;
            }
        }


        public void mapWords(string fileText)
        {
            Parallel.ForEach(produceWordBlocks(fileText), wordBlock =>
            {   //split the block into words
                string[] words = wordBlock.Split(' ');
                StringBuilder wordBuffer = new StringBuilder();

                //cleanup each word and map it
                foreach (string word in words)
                {   //Remove all spaces and punctuation
                    foreach (char c in word)
                    {
                        if (char.IsLetterOrDigit(c) || c == '\'' || c == '-')
                            wordBuffer.Append(c);
                    }
                    //Send word to the wordChunks Blocking Collection
                    if (wordBuffer.Length > 0)
                    {
                        wordChunks.Add(wordBuffer.ToString());
                        wordBuffer.Clear();
                    }
                }
            });

            wordChunks.CompleteAdding();
        }

        public ConcurrentDictionary<string, int> wordStore = new ConcurrentDictionary<string, int>();

        public void reduceWords()
        {
            Parallel.ForEach(wordChunks.GetConsumingEnumerable(), word =>
            {   //if the word exists, use a thread safe delegate to increment the value by 1
                //otherwise, add the word with a default value of 1
                wordStore.AddOrUpdate(word, 1, (key, oldValue) => Interlocked.Increment(ref oldValue));
            });
        }

        public void mapReduce(string fileText)
        {   //Reset the Blocking Collection, if already used
            if (wordChunks.IsAddingCompleted)
            {
                wordBag = new ConcurrentBag<string>();
                wordChunks = new BlockingCollection<string>(wordBag);
            }

            //Create background process to map input data to words
            System.Threading.ThreadPool.QueueUserWorkItem(delegate(object state)
            {
                mapWords(fileText);
            });

            //Reduce mapped words
            reduceWords();
        }

    }

    class Program
    {
        static void Main(string[] args)
        {
        }
    }
}
