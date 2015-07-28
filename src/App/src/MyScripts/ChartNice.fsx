#I "../packages/FSharp.Charting.0.90.12"
#r "../FSharp.AsyncExtensions/bin/FSharp.AsyncExtensions.dll"
#load "FSharp.Charting.fsx"
#load "EventEx.fsx"

open System
open System.IO

open FSharp.Charting
open FSharp.Control

open FSharp.Charting
open FSharp.Control
open System
open System.Net
open System.Text
open FSharp.IO
open FSharp.Control
open System.IO
open System.Drawing

let timeSeriesData = 
  [ for x in 0 .. 99 -> 
      DateTime.Now.AddDays (float x),sin(float x / 10.0) ]
let rnd = new System.Random()
let rand() = rnd.NextDouble()

let data = [ for x in 0 .. 99 -> (x,x*x) ]
let data2 = [ for x in 0 .. 99 -> (x,sin(float x / 10.0)) ]
let data3 = [ for x in 0 .. 99 -> (x,cos(float x / 10.0)) ]
let incData = Event.clock 10 |> Event.map (fun x -> (x, x.Millisecond))

// Cycle through two data sets of the same type
LiveChart.Line (Event.c 1000 [data2; data3])

LiveChart.LineIncremental(incData,Name="MouseMove")
  .WithXAxis(Enabled=false).WithYAxis(Enabled=false)

LiveChart.FastLineIncremental(incData,Name="MouseMove")
  .WithXAxis(Enabled=false).WithYAxis(Enabled=false)



type System.IO.StreamReader with
  member x.AsyncReadLine() = 
    x.ReadLineAsync() |> Async.AwaitTask

type FSharp.Charting.LiveChart with
  static member NiceLine(data) = 
    let grid = ChartTypes.Grid(LineColor=System.Drawing.Color.LightGray)
    data 
    |> AsyncSeq.toObservable
    |> Observable.windowed 20
    |> LiveChart.Line
    |> Chart.WithYAxis(Min=20.0, Max=45.0)
    |> Chart.WithYAxis(MajorGrid=grid)
    |> Chart.WithXAxis(MajorGrid=grid)

type System.IO.StreamReader with
  member reader.AsyncReadLines() = asyncSeq {
    let! line = reader.AsyncReadLine()
    if line <> null then
      yield line 
      yield! reader.AsyncReadLines() }

/////////////////////////
type FSharp.Charting.LiveChart with
  static member NiceLine (data1, data2) =
    let grid = ChartTypes.Grid(LineColor=System.Drawing.Color.LightGray)
    let d1 = 
        data1 
        |> AsyncSeq.toObservable
        |> Observable.windowed 20
        |> LiveChart.Line
    let d2 = 
        data2 
        |> AsyncSeq.toObservable
        |> Observable.windowed 20
        |> LiveChart.Line
    Chart.Combine(
        [ d1 ; d2 ] )
        |> Chart.WithYAxis(Min=20.0, Max=45.0)
        |> Chart.WithYAxis(MajorGrid=grid)
        |> Chart.WithXAxis(MajorGrid=grid)


//type System.IO.StreamReader with
//  member reader.AsyncCsvReadLines() = seq {
//    let line = reader.ReadLine()
//    if line <> null then
//      yield line 
//      yield reader.ReadLine() }



// --------------------------------------------------------
// Using asynchronous sequences
// --------------------------------------------------------

let goog = @"C:\Data\csv\goog.csv"
let msft = @"C:\Data\csv\msft.csv"

let data (file:string) = asyncSeq {
  let reader = new StreamReader(file)
  for line in reader.AsyncReadLines() |> AsyncSeq.skip 1 do
    do! Async.Sleep(100)
    let parts = line.Split(',')
    let date, price = DateTime.Parse(parts.[0]), float parts.[1]
    yield date, price }

LiveChart.NiceLine (data(goog), data(msft))

