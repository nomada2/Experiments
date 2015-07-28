#r "System.Drawing.dll"
#load "ImageTool.fsx"

open System
open System.Text
open System.Drawing
open System.Drawing.Imaging
open ImageTool 

type HistogramColor =
    | Red of byte array
    | Green of byte array
    | Blue of byte array

type HistogramColorRec = {  Red:byte array
                            Green:byte array
                            Blue:byte array }


let calculateHistogram (bitmap:Bitmap) =
    let red = Array.zeroCreate<byte> 256
    let green = Array.zeroCreate<byte> 256
    let blue = Array.zeroCreate<byte> 256
    let newBitmap = bitmap.resize(16, 16)
    for x = 0 to newBitmap.Width - 1 do
        for y = 0 to newBitmap.Height - 1 do
            let color = newBitmap.GetPixel(x, y)
            red.[int color.R] <- red.[int color.R] + 1uy
            green.[int color.G] <- green.[int color.G] + 1uy
            blue.[int color.B] <- blue.[int color.B] + 1uy
    {Red=red; Green=green; Blue=blue}

type Histogram(bitmap:Bitmap) =

    let histogram = calculateHistogram(bitmap)
    let p = [| Pens.Red; Pens.Green; Pens.Blue |]

    member x.Red = histogram.Red
    member x.Blue = histogram.Blue
    member x.Green = histogram.Green
    member x.Bitmap = bitmap

    member x.GetVarianceBetweenHistogram (histogram:HistogramColorRec) =
        let rec variance diffRed diffGreen diffBlue i =
            match i with
            | _ when i < 256 -> (diffRed, diffGreen, diffBlue)
            | i ->  let diffRed' = diffRed + Math.Pow(float(x.Red.[i] - histogram.Red.[i]), 2.)
                    let diffGreen' = diffGreen + Math.Pow(float(x.Green.[i] - histogram.Green.[i]), 2.)
                    let diffBlue' = diffBlue + Math.Pow(float(x.Blue.[i] - histogram.Blue.[i]), 2.)
                    variance diffRed' diffGreen' diffBlue' (i + 1)
        let (diffRed, diffGreen, diffBlue) = variance 0. 0. 0. 0
        let diffRed' =  diffRed / 256.
        let diffGreen' = diffGreen / 256.
        let diffBlue' = diffBlue / 256.
        let maxDiff = 512.
        (diffRed' / maxDiff + diffGreen' / maxDiff + diffBlue' / maxDiff) / 3.

    member x.Visualize() =
        let oneColorHeight = 100
        let margin = 10
        let max a = a |> Array.max
        let maxValues = [| (max x.Red); (max x.Green); (max x.Blue) |]
        let values = [| x.Red; x.Green; x.Blue |]
        let histogramBitmap = new Bitmap(276, oneColorHeight * 3 + margin * 4)
        let g = Graphics.FromImage(histogramBitmap)
        g.FillRectangle(Brushes.White, 0, 0, histogramBitmap.Width, histogramBitmap.Height)
        let yOffset = margin + oneColorHeight

        for i = 0 to 255 do
            for color = 0 to 2 do
                 g.DrawLine(p.[color], margin + i, yOffset * (color + 1), margin + i, yOffset * (color + 1) - (int(values.[color].[i]) / int(maxValues.[color])) * oneColorHeight)

        for i = 0 to 3 do
            g.DrawString(p.[i].Color.ToKnownColor().ToString() + ", max value: " + maxValues.[i].ToString(), SystemFonts.SmallCaptionFont, Brushes.Silver, float32(margin + 11), float32(yOffset * i + margin + margin + 1))
            g.DrawString(p.[i].Color.ToKnownColor().ToString() + ", max value: " + maxValues.[i].ToString(), SystemFonts.SmallCaptionFont, Brushes.Black, float32(margin + 10), float32(yOffset * i + margin + margin))
            g.DrawRectangle(p.[i], margin, yOffset * i + margin, 256, oneColorHeight)
        g.Dispose()
        histogramBitmap

    override x.ToString() =
        let rec buildReadableRGB (sb:StringBuilder) i =
            match i with 
            | _ when i < 256 -> sb.ToString()
            | _ ->  buildReadableRGB (sb.AppendFormat("RGB {0,3} : ({0,3},{1,3},{2,3})", i, x.Red.[i], x.Green.[i], x.Blue.[i])) (i + 1)
        buildReadableRGB (new StringBuilder()) 0


let dirImages = __SOURCE_DIRECTORY__ + @"\Images\"
let file = dirImages + "bacon.bmp"
System.IO.File.Exists file

let bmp = new Bitmap(file)
let hs = new Histogram(bmp)
let b = hs.Visualize()


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
open System.Drawing
open System.Windows.Media.Imaging

type WImage = System.Windows.Controls.Image

type ImageComparer(filePathImage1:string,filePathImage2:string) as ic = 
    inherit Window()

    let image1 = new WImage(HorizontalAlignment=HorizontalAlignment.Stretch, Stretch=Stretch.Uniform, VerticalAlignment=VerticalAlignment.Stretch)
    let image2 = new WImage(HorizontalAlignment=HorizontalAlignment.Stretch, Stretch=Stretch.Uniform, VerticalAlignment=VerticalAlignment.Stretch)
    let imgDifference = new WImage(HorizontalAlignment=HorizontalAlignment.Stretch, Stretch=Stretch.Uniform, VerticalAlignment=VerticalAlignment.Stretch)

    do
        ic.Topmost <- true
        let grid = new Grid(UseLayoutRounding=true)
        grid.ColumnDefinitions.Add(new ColumnDefinition())
        grid.ColumnDefinitions.Add(new ColumnDefinition())
        grid.ColumnDefinitions.Add(new ColumnDefinition())
    
        let bitmapSource1 = new System.Windows.Media.Imaging.BitmapImage(new Uri(filePathImage1))
        let bitmapSource2 = new System.Windows.Media.Imaging.BitmapImage(new Uri(filePathImage2))
        image1.Source <- bitmapSource1
        image2.Source <- bitmapSource2

        Grid.SetColumn(image1, 0)
        Grid.SetColumn(imgDifference, 1)
        Grid.SetColumn(image2, 2)
        grid.Children.Add(image1) |> ignore
        grid.Children.Add(imgDifference) |> ignore
        grid.Children.Add(image2) |> ignore
        ic.Content <- grid
    
    let getBitmapImage2Bitmap(image:System.Windows.Media.Imaging.BitmapImage) =
        use ms = new System.IO.MemoryStream()
        let encoder = new System.Windows.Media.Imaging.PngBitmapEncoder() :> BitmapEncoder
        encoder.Frames.Add(System.Windows.Media.Imaging.BitmapFrame.Create(image))
        encoder.Save(ms)
        (new Bitmap(ms))
                
    let getBitmapImageFromBytes(bytes:byte array) =
        use ms = new System.IO.MemoryStream(bytes)
        let bitmapImage = new BitmapImage()
        bitmapImage.BeginInit()
        bitmapImage.StreamSource <- ms
        bitmapImage.CacheOption <- BitmapCacheOption.OnLoad
        bitmapImage.EndInit()
        bitmapImage

    let getBitmap2BitmapImage (image:System.Drawing.Bitmap) =
        use ms = new System.IO.MemoryStream()
        image.Save(ms, ImageFormat.Png)
        let imageData = ms.ToArray()
        let bmImag = getBitmapImageFromBytes imageData
        bmImag

    member x.CompareImages() =        
        let img1 = getBitmapImage2Bitmap (image1.Source :?> BitmapImage)
        let img2 = getBitmapImage2Bitmap (image2.Source :?> BitmapImage)            
        let diffImage = img1.getDifferenceImage(img2, Some(true))
                
        let bitmapDiff = fst diffImage                
        imgDifference.Source <- getBitmap2BitmapImage bitmapDiff
            

let dirImages = __SOURCE_DIRECTORY__ + @"\Images\"
let file1 = dirImages + "picasso.bmp"
let file2 = dirImages + "bacon.bmp"

let window = new ImageComparer(file1, file2)
window.Show()
window.CompareImages()
    
#load ".\Common\show.fs"
showImage <| System.Drawing.Bitmap.FromFile(file1)