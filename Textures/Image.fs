module ImageTexture
open System
open Texture
open SixLabors.ImageSharp
open CommonTypes
open SixLabors.ImageSharp.PixelFormats
open Hopac
open HttpFs.Client
open System.IO

let loadHttp url = 
  let response = Request.createUrl Get url |> getResponse |> run
  response.body

let load uri = 
    if (Uri(uri).IsFile) then 
        File.OpenRead uri :> Stream
    else
        loadHttp uri :> Stream

let image file :Texture = 
    use stream = load file
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
