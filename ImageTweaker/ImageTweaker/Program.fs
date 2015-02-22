﻿module ImageTweaker

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





[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
