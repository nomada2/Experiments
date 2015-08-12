////#load "Library1.fs"
////open Graph
#r "PresentationCore.dll"
#r "PresentationFramework.dll"
#r "System.Xaml.dll"
#r "WindowsBase.dll"
#r "UIAutomationTypes.dll"

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Media
open System.Windows.Shapes
open System.Windows.Data
open System.Threading.Tasks
open System.Windows.Threading

type CompleteGraph() as cg =
  inherit UserControl()

  let dispatcher = Dispatcher.CurrentDispatcher
 
  let shape = Polyline()
  do
    let vb = Viewbox()
    cg.Content <- vb;
    vb.Child <- shape
    shape.Stroke <- SolidColorBrush Colors.Black
    shape.StrokeThickness <- 0.001
 
  let drawEdges vertices =
    shape.Points.Clear()
    let n = vertices
    let p i =
      let t = float(i % n) / float n * 2.0 * Math.PI
      Point(1.0 + sin t, 1.0 + cos t)
  
    for i=0 to n-1 do
      for j=i+1 to n do
        Seq.iter shape.Points.Add [p i; p j]

    
    Parallel.For(0, n-1, Action<_>(fun i ->
        dispatcher.BeginInvoke(new Action<_>(fun () ->
         for j=i+1 to n do
            Seq.iter shape.Points.Add [p i; p j] )) |> ignore
            )) |> ignore

  member cg.Vertices
    with set v = drawEdges v

let graphController = CompleteGraph()
let window = new Window(Topmost=true)
window.Content <- graphController
window.Show()

let interval = 500.
//Window(Content=Controls.Viewbox(Child=shape)) |> Application().Run |> ignoref;f;
let timer = new System.Windows.Threading.DispatcherTimer() 
timer.Interval <- TimeSpan.FromMilliseconds(interval)
timer.Tick 
    |> Observable.scan(fun (_, v) _ -> 
                let currentVer = abs(v - DateTime.Now.Millisecond) 
                match v with
                | 99 -> (true, 1)
                | v when currentVer > int interval -> (true, (v + 1))
                | _ -> (false, v))
       (true, 1)
    |> Observable.filter fst
    |> Observable.add(fun (_, v) -> 
        printfn "Vertices %d" v
        graphController.Vertices <- v)

//timer.Start()
//timer.Stop()

 