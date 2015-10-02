module ImageTweaker

open System
open System.IO
open System.Drawing
open System.Windows.Forms

let form = new Form(Visible = true, Text = "3D Image", 
                    TopMost = true, Size = Size(600,600))
let imageCtrl = new System.Windows.Forms.PictureBox(Dock = DockStyle.Fill)
form.Controls.Add(imageCtrl)
let show image =
    imageCtrl.Image <- image
    System.Windows.Forms.Application.DoEvents()

let root = __SOURCE_DIRECTORY__
let images = Path.Combine(root,"Images")
let combine imageName =  Path.Combine(images, imageName)

// You can create a 3D effect by taking each pixel from the source image (removing the red) 
// and the pixel twenty places to its left (removing the blue and green) and blending the two together
let convertImageTo3D (imagePath:string) =
    let bitmap = Bitmap.FromFile(imagePath) :?> Bitmap
    let w,h = bitmap.Width, bitmap.Height
    for x in 20 .. (w-1) do
        for y in 0 .. (h-1) do
            let c1 = bitmap.GetPixel(x,y)
            let c2 = bitmap.GetPixel(x - 20,y)
            let color3D = Color.FromArgb(int 0,int c1.R, int c2.G, int c2.B)
            bitmap.SetPixel(x - 20 ,y,color3D)
    bitmap

let imageTweaked = combine "davinci.bmp" |> convertImageTo3D 
show <| imageTweaked

let tridiagonal (n:int) (c1:double array) (c2:double array) (c3:double array) (derivate:double array) =
    
    let tol = 1.0e-12
    let mutable isSingualr = (c2.[0] < tol)

    for i = 1 to n - 1  do //&& (not isSingualr) do
        c1.[i] <- c1.[i] / c2.[i - 1]
        c2.[i] <- c2.[i] - c1.[i] * c3.[i - 1]
        isSingualr <- (c2.[i] < tol)
        derivate.[i] <- derivate.[i] - c1.[i] * derivate.[i - 1]

    if (not isSingualr) then
        derivate.[n - 1] <- derivate.[n - 1] / c2.[n - 1]
        for i = n - 2 downto 0 do
            derivate.[i] <- (derivate.[i] - c3.[i] * derivate.[i + 1]) / c2.[i]
        Some(derivate)
    else
        None


let secondDerivatives (xarray:double array) (yarray:double array) = //: double array =

    let n = xarray.Length
    let c1 = Array.zeroCreate<double> n
    let c2 = Array.zeroCreate<double> n
    let c3= Array.zeroCreate<double> n
    let dx = Array.zeroCreate<double> n
    let derivate = Array.zeroCreate<double> n

    for i = 1 to n - 1 do
        dx.[i] <- xarray.[i] - xarray.[i - 1]
        derivate.[i] <- (yarray.[i] - yarray.[i - 1]) / dx.[i]

    for i = 1 to n - 1 do
        c2.[i - 1] <- 2.
        c3.[i - 1] <- dx.[i + 1] / (dx.[i] + dx.[i + 1])
        c1.[i - 1] <- 1. - c3.[i - 1]
        derivate.[i - 1] <- 6. * (derivate.[i - 1] - derivate.[i]) / (dx.[i] - dx.[i + 1])

    tridiagonal (n - 2) c1 c2 c3 derivate



[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
