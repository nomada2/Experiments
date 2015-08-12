﻿using System;
using System.Collections.Concurrent;
using System.Linq;
using System.Net;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;

// Demonstrates how to create a basic dataflow pipeline. 
// This program downloads the book "The Iliad of Homer" by Homer from the Web 
// and finds all reversed words that appear in that book. 
class DataflowReversedWords
{
    static void Main(string[] args)
    {
        // 
        // Create the members of the pipeline. 
        //  

        //var nums = Enumerable.Range(0, 10).ToList();//.AsQueryable();

        //var query = (from num in nums.AsQueryable()
        //             where num % 2 == 0
        //             select num * num).Sum();


        //int sum = 0;
        //for (int index = 0; index < nums.Count; index++)
        //{
        //    int num = nums[index];
        //    if (num % 2 == 0)
        //        sum += num * num;
        //}




        // Downloads the requested resource as a string. 
        var downloadString = new TransformBlock<string, string>(uri =>
        {
            Console.WriteLine("Downloading '{0}'...", uri);

            return new WebClient().DownloadString(uri);
        });

        // Separates the specified text into an array of words. 
        var createWordList = new TransformBlock<string, string[]>(text =>
        {
            Console.WriteLine("Creating word list...");

            // Remove common punctuation by replacing all non-letter characters  
            // with a space character to. 
            char[] tokens = text.ToArray();
            for (int i = 0; i < tokens.Length; i++)
            {
                if (!char.IsLetter(tokens[i]))
                    tokens[i] = ' ';
            }
            text = new string(tokens);

            // Separate the text into an array of words. 
            return text.Split(new char[] { ' ' },
               StringSplitOptions.RemoveEmptyEntries);
        });

        // Removes short words, orders the resulting words alphabetically,  
        // and then remove duplicates. 
        var filterWordList = new TransformBlock<string[], string[]>(words =>
        {
            Console.WriteLine("Filtering word list...");

            return words.Where(word => word.Length > 3).OrderBy(word => word)
               .Distinct().ToArray();
        });

        // Finds all words in the specified collection whose reverse also  
        // exists in the collection. 
        var findReversedWords = new TransformManyBlock<string[], string>(words =>
        {
            Console.WriteLine("Finding reversed words...");

            // Holds reversed words. 
            var reversedWords = new ConcurrentQueue<string>();

            // Add each word in the original collection to the result whose  
            // reversed word also exists in the collection.
            Parallel.ForEach(words, word =>
            {
                // Reverse the work. 
                string reverse = new string(word.Reverse().ToArray());

                // Enqueue the word if the reversed version also exists 
                // in the collection. 
                if (Array.BinarySearch<string>(words, reverse) >= 0 &&
                    word != reverse)
                {
                    reversedWords.Enqueue(word);
                }
            });

            return reversedWords;
        });

        // Prints the provided reversed words to the console.     
        var printReversedWords = new ActionBlock<string>(reversedWord =>
        {
            Console.WriteLine("Found reversed words {0}/{1}",
               reversedWord, new string(reversedWord.Reverse().ToArray()));
        });

        // 
        // Connect the dataflow blocks to form a pipeline. 
        //

        downloadString.AsObservable().Subscribe(i => 
            Console.WriteLine("Action Sub {0}", i));

        var te= downloadString.AsObserver();


        downloadString.LinkTo(createWordList);
        createWordList.LinkTo(filterWordList);
        filterWordList.LinkTo(findReversedWords);
        findReversedWords.LinkTo(printReversedWords);

        // 
        // For each completion task in the pipeline, create a continuation task 
        // that marks the next block in the pipeline as completed. 
        // A completed dataflow block processes any buffered elements, but does 
        // not accept new elements. 
        //

        downloadString.Completion.ContinueWith(t =>
        {
            if (t.IsFaulted) ((IDataflowBlock)createWordList).Fault(t.Exception);
            else createWordList.Complete();
        });
        createWordList.Completion.ContinueWith(t =>
        {
            if (t.IsFaulted) ((IDataflowBlock)filterWordList).Fault(t.Exception);
            else filterWordList.Complete();
        });
        filterWordList.Completion.ContinueWith(t =>
        {
            if (t.IsFaulted) ((IDataflowBlock)findReversedWords).Fault(t.Exception);
            else findReversedWords.Complete();
        });
        findReversedWords.Completion.ContinueWith(t =>
        {
            if (t.IsFaulted) ((IDataflowBlock)printReversedWords).Fault(t.Exception);
            else printReversedWords.Complete();
        });

        // Process "The Iliad of Homer" by Homer.
        downloadString.Post("http://www.gutenberg.org/files/6130/6130-0.txt");

        // Mark the head of the pipeline as complete. The continuation tasks  
        // propagate completion through the pipeline as each part of the  
        // pipeline finishes.
        downloadString.Complete();

        // Wait for the last block in the pipeline to process all messages.
        printReversedWords.Completion.Wait();

        Console.ReadLine();

        te.OnNext("ciao");
        Console.ReadLine();

    }
}