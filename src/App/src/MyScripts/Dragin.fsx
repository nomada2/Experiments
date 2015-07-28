#r "System.dll"
#r "System.Windows.Forms.dll"
#r "PresentationFramework.dll"
#r "PresentationCore.dll"
#r "WindowsBase.dll"

open System.Windows
open System.Windows.Media
open System.Windows.Shapes

let m = Matrix(0.0, 0.5, -0.5, 0.0, 0.0, 0.0)

let step segs =
  seq { for a: Point, b: Point in segs do
          let x = a + 0.5 * (b - a) + (b - a) * m
          yield! [a, x; b, x] }

let rec nest n f x =
  if n=0 then x else nest (n-1) f (f x)

[<System.STAThread>]
do
  let ps = nest 14 step (seq [Point(0., 0.), Point(1., 0.)])
  let d = Vector(Seq.min[for a, b in ps -> min a.X b.X], Seq.min[for a, b in ps -> min a.Y b.Y])
  let lineTo p = (LineSegment(p, true) :> PathSegment)
  let path = Shapes.Path(Stroke=Brushes.Black, StrokeThickness=0.003)
  path.Data <- PathGeometry[for a, b in ps -> PathFigure(a-d, [lineTo(b-d)], false)]
  (Application()).Run(Window(Content=Controls.Viewbox(Child=path))) |> ignore
