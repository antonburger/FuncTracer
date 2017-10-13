module ImageTexture
open System
open Texture
open SixLabors.ImageSharp
open CommonTypes
open SixLabors.ImageSharp.PixelFormats

let image (file:string):Texture = 
    use stream = System.IO.File.OpenRead file
    use image = Image.Load<Rgb24> stream
    let pixels = image.SavePixelData()
    let width = image.Width
    let height = image.Height
    (fun coords ->
        let (u,v) = repeat coords
        let x = Operators.floor (u*(float)width) |> Convert.ToInt32
        let y = Operators.floor (v*(float)height) |> Convert.ToInt32
        let index = y*(3*width)+(3*x)
        let r = pixels.[index]
        let g = pixels.[index+1]
        let b = pixels.[index+2]
        Colour (float(r)/255.0, float(g)/255.0, float(b)/255.0)
    )
