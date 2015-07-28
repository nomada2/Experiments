#r "System.Drawing.dll"

open System
open System.Collections.Generic
open System.Drawing.Drawing2D
open System.Drawing.Imaging
open System.Drawing
open System.IO
open System.Runtime.InteropServices


//type ImageTool =
//    
//    static member private brushesLazy = lazy Array.init 256 (fun i -> new SolidBrush(Color.FromArgb(255, i, i / 3, i / 2)))
//
//    static member brushes = ImageTool.brushesLazy.Value
//        
//    static member grayScakeMatrix =
//        let arrColorMatrix = Array.init 5 (fun _ -> Array.zeroCreate<float32> 5)
//        arrColorMatrix.[0] <- [|0.3f; 0.3f; 0.3f; 0.f; 0.f|]
//        arrColorMatrix.[1] <- [|0.59f; 0.59f; 0.59f; 0.f; 0.f|]
//        arrColorMatrix.[2] <- [|0.11f; 0.11f; 0.11f; 0.f; 0.f|]
//        arrColorMatrix.[3] <- [|0.f; 0.f; 0.f; 1.f; 0.f|]
//        arrColorMatrix.[4] <- [|0.f; 0.f; 0.f; 0.f; 1.f;|]
//        (new ColorMatrix(arrColorMatrix))
//
//    static member flatMatrix (array2D:'a [,]) =        
//        let rec flat (arr:'a [,]) i acc = 
//            if Array2D.length1 arr = i then acc
//            else
//                flat arr (i + 1) acc @ (arr.[i, 0..(Array2D.length2 arr - 1)] |> Array.toList)
//        flat array2D 0 [] |> List.toArray
//
//
//

module ImageTool = 
    let private brushesLazy = lazy Array.init 256 (fun i -> new SolidBrush(Color.FromArgb(255, i, i / 3, i / 2)))
    let brushes = brushesLazy.Value
       
    let grayScakeMatrix =
        let arrColorMatrix = Array.init 5 (fun _ -> Array.zeroCreate<float32> 5)
        arrColorMatrix.[0] <- [|0.3f; 0.3f; 0.3f; 0.f; 0.f|]
        arrColorMatrix.[1] <- [|0.59f; 0.59f; 0.59f; 0.f; 0.f|]
        arrColorMatrix.[2] <- [|0.11f; 0.11f; 0.11f; 0.f; 0.f|]
        arrColorMatrix.[3] <- [|0.f; 0.f; 0.f; 1.f; 0.f|]
        arrColorMatrix.[4] <- [|0.f; 0.f; 0.f; 0.f; 1.f;|]
        (new ColorMatrix(arrColorMatrix))

    let flatMatrix (array2D:'a [,]) =  seq {
        for x = 0 to (Array2D.length1 array2D - 1) do
            for y = 0 to (Array2D.length2 array2D - 1) do
                yield array2D.[x,y]
        }      

    /// Implements a fast unsafe conversion from 2D array to a bitmap     
    /// Converts array to a bitmap using the provided conversion function,
    /// which converts value from the array to a color value
    let toBitmap f (arr:_[,]) =
      // Create bitmap & lock it in the memory, so that we can access it directly
      let bmp = new Bitmap(arr.GetLength(0), arr.GetLength(1))
      let rect = new Rectangle(0, 0, bmp.Width, bmp.Height)
      let bmpData = 
        bmp.LockBits
          (rect, Imaging.ImageLockMode.ReadWrite, 
           Imaging.PixelFormat.Format32bppArgb)
       
      // Using pointer arithmethic to copy all the bits
      let ptr0 = bmpData.Scan0 
      let stride = bmpData.Stride
      for i = 0 to bmp.Width - 1 do
        for j = 0 to bmp.Height - 1 do
          let offset = i*4 + stride*j
          let clr = (f(arr.[i,j]) : Color).ToArgb()
          Marshal.WriteInt32(ptr0, offset, clr)
  
      bmp.UnlockBits(bmpData)
      bmp



type System.Drawing.Image with
    member x.resize(width:int, height:int) =
        let smallImage = new Bitmap(width, height)
        use g = Graphics.FromImage(smallImage)
        g.SmoothingMode <- SmoothingMode.HighQuality
        g.InterpolationMode <- InterpolationMode.HighQualityBicubic
        g.PixelOffsetMode <- PixelOffsetMode.HighQuality
        g.DrawImage(x, 0, 0, width, height)
        smallImage

    member x.getGrayScaleVersion() =
        let arrColorMatrix = Array.init 5 (fun _ -> Array.zeroCreate<float32> 5)
        arrColorMatrix.[0] <- [|0.3f; 0.3f; 0.3f; 0.f; 0.f|]
        arrColorMatrix.[1] <- [|0.59f; 0.59f; 0.59f; 0.f; 0.f|]
        arrColorMatrix.[2] <- [|0.11f; 0.11f; 0.11f; 0.f; 0.f|]
        arrColorMatrix.[3] <- [|0.f; 0.f; 0.f; 1.f; 0.f|]
        arrColorMatrix.[4] <- [|0.f; 0.f; 0.f; 0.f; 1.f;|]
        let image = new Bitmap(x.Width, x.Height)
        let g = Graphics.FromImage(image)
        let attributes = new ImageAttributes()
        attributes.SetColorMatrix(new ColorMatrix(arrColorMatrix))
        g.DrawImage(x, new Rectangle(0, 0, x.Width, x.Height), 0, 0, x.Width, x.Height, GraphicsUnit.Pixel, attributes)
        g.Dispose()
        image


    (* Finds the differences between two images and returns them in a doublearray
       The differences between the two images as a doublearray *)
    member __.getDifferences(image:Image) =
        let firstImage = __.resize(16, 16).getGrayScaleVersion()
        let secondImage = image.resize(16, 16).getGrayScaleVersion()
        let differences = Array2D.init 16 16 (fun x y -> 
            byte(Math.Abs(int (firstImage.GetPixel(x, y).R) - int (secondImage.GetPixel(x, y).R))))
        differences
       
    member x.percentageDifference(image:Image, threshold:byte option) =
        let threshold = defaultArg threshold 3uy
        let differences = x.getDifferences(image)
        let rec flat (arr:byte [,]) i acc = 
            if Array2D.length1 arr = i then acc
            else
                flat arr (i + 1) acc @ (arr.[i, 0..(Array2D.length2 arr - 1)] |> Array.toList)
        let diffPixel =              
            flat differences 0 []
            |> Seq.filter(fun b -> b > threshold)
            |> Seq.length
        float32(diffPixel) / 256.f
        
    member x.getDifferenceImage(image:Image, adjustColorSchemeToMaxDifferenceFound:bool option) =
        let adjustColorSchemeToMaxDifferenceFound = defaultArg adjustColorSchemeToMaxDifferenceFound false
        let cellSize = 16
        let blockPixels = 16 * cellSize + 1
        let bmp = new Bitmap(blockPixels, blockPixels)
        let g = Graphics.FromImage(bmp)
        g.FillRectangle(Brushes.Black, 0, 0, bmp.Width, bmp.Height)
        let differences = x.getDifferences(image)
       
        let rec getMaxDifference (differences:byte list) (maxDifference:byte) =
            match differences with
            | [] -> if maxDifference = 0uy then 1uy else maxDifference
            | b::rest -> if b > maxDifference then getMaxDifference rest b
                            else getMaxDifference rest maxDifference
        let maxDifference = 
            if adjustColorSchemeToMaxDifferenceFound then 
                getMaxDifference ((differences |> ImageTool.flatMatrix) |> Seq.toList) 0uy
            else 255uy

        let font= new  Font( "Arial " , 9.75F,FontStyle.Regular, GraphicsUnit.Point)   
        
        for y = 0 to differences.GetLength(1) - 1 do
            for x = 0 to differences.GetLength(0) - 1 do
                let cellValue = differences.[x,y]
                let cellText = string cellValue
                let percentageDifference = float(differences.[x,y] / maxDifference)
                let colorIndex = int(255. * percentageDifference)
                
                g.FillRectangle(ImageTool.brushes.[colorIndex], x * cellSize, y * cellSize, cellSize, cellSize)
                g.DrawRectangle(Pens.Blue, x * cellSize, y * cellSize, cellSize, cellSize)
                let size = g.MeasureString(cellText, font)
                g.DrawString(cellText, font, Brushes.Black, float32(x * cellSize + cellSize / 2 - int(size.Width) / 2 + 1),  float32(y * cellSize + cellSize / 2 - int(size.Height) / 2 + 1))
                g.DrawString(cellText, font, Brushes.White, float32(x * cellSize + cellSize / 2 - int(size.Width) / 2), float32(y * cellSize + cellSize / 2 - int(size.Height) / 2))
        (bmp, differences)


//
//let flat2DArray (array2d:'a [,]) =
//    let rec flat2DArray (array2d:'a [,]) i (array1d:'a array) =
//        match  Array2D.length1 array2d with 
//        | x when i = x -> array1d
//        | _ -> flat2DArray array2d (i + 1) 
//                           array1d 
//                           |> Array.append array2d.[i, 0..(Array2D.length2 array2d - 1)]
//    flat2DArray array2d 0 [||]


