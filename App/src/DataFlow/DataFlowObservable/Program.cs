using System;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using System.Reactive;
using System.Reactive.Concurrency;
using System.Reactive.Linq;
using System.Diagnostics;

namespace DataFlowObservable
{
	class MainClass
	{
		
		public static void Main (string[] args)
		{

			var testObs = new TransformBlock<int, int>(item => item * item);
			var o = testObs.AsObservable ();

			o.Subscribe(i=> Console.WriteLine("Test Obs received {0}", i.ToString()));

			for (int i = 0; i < 5; i++) {
				testObs.Post (i);
			}

			testObs.Completion.Wait ();


			Console.ReadKey ();

			var buffer = new BufferBlock<int>();
			IObservable<int> integers = buffer.AsObservable();
			integers.Subscribe(data => 
				Console.WriteLine(data),
				ex => Console.WriteLine(ex),
				() => Console.WriteLine("Done"));

			buffer.Post(13);
			buffer.Post(14);
			buffer.Post(15);
			buffer.Post(16);

			Console.ReadKey ();

			IObservable<DateTimeOffset> ticks =
				Observable.Interval(TimeSpan.FromSeconds(1))
					.Timestamp()
					.Select(x => x.Timestamp)
					.Take(5);

			var display = new ActionBlock<DateTimeOffset>(x => Console.WriteLine(x));
			ticks.Subscribe(display.AsObserver());

			Console.ReadKey ();


			try
			{
				display.Completion.Wait();
				Trace.WriteLine("Done.");
			}
			catch (Exception ex)
			{
				Trace.WriteLine(ex);
			}

			try
			{
				var multiplyBlock = new TransformBlock<int, int>(item =>
					{
					//	if (item == 1)
					//		throw new InvalidOperationException("Blech.");

			     		return item * 2;
					});
				
				var subtractBlock = new TransformBlock<int, int>(item => item - 2);
				multiplyBlock.LinkTo(subtractBlock);

					

				var printing = new ActionBlock<int>(i => Console.WriteLine("Received {0}", i.ToString()));

				subtractBlock.LinkTo(printing, new DataflowLinkOptions { PropagateCompletion = true });

				IObservable<int> obs = subtractBlock.AsObservable();
				obs.Subscribe(i => Console.WriteLine("Received Observable {0}", i.ToString()));


				for(int i = 0; i < 10;i++){
					multiplyBlock.Post(i);
					subtractBlock.Post(i);
				}

				Console.ReadLine ();
				//printing.Completion.Wait();
			}

			catch (AggregateException exception)
			{
				AggregateException ex = exception.Flatten();
				System.Diagnostics.Trace.WriteLine(ex.InnerException);
			}

			Console.WriteLine ("Hello World!");

			Console.ReadLine ();
		}
	}
}

