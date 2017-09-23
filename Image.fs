module Image

open System.IO
open Ray
open Vector

type Colour = Colour of (byte * byte * byte)

type Resolution = Resolution of (int * int)

type Bitmap = { resolution: Resolution; pixels : Colour list }

let resH (Resolution r) = fst r
let resV (Resolution r) = snd r

let write b fn =
    use stream = new FileStream(fn, FileMode.Create, FileAccess.Write, FileShare.None)
    use writer = new StreamWriter(stream, System.Text.Encoding.ASCII)
    let rec writePixels = function
        | [] -> ()
        | Colour (r,g,b)::ps ->
            writer.WriteLine("{0} {1} {2}", r, g, b)
            writePixels ps
    writer.WriteLine("P3")
    writer.WriteLine("{0} {1}", resH b.resolution, resV b.resolution)
    writer.WriteLine("255")
    writePixels b.pixels

type Frame = { i : Vector; j : Vector; k : Vector }

let fromCamera c =
    // Camera's a left-handed coordinate system (+x right, +y up, +z forward). So right = up x forward rather than forward x up.
    let k = subP c.lookAt c.o |> normalise
    let i = cross c.up k |> normalise
    let j = cross k i
    { i = i; j = j; k = k }

type PixelGrid = {
    centre: Point;
    i: Vector;
    j: Vector;
    resolution: Resolution;
    pixelSize: float * float;
    pixelCentres: (float * float) [,];
}

let createGrid camera resolution =
    let frame = fromCamera camera
    let ahead = addP camera.o frame.k
    let halfY = System.Math.Tan(camera.fovY / 2.0)
    let halfX = halfY * camera.aspectRatio
    let incY = (halfY * 2.0) / (resH resolution - 1 |> float)
    let incX = (halfX * 2.0) / (resV resolution - 1 |> float)
    let offsets = Array2D.init (resH resolution) (resV resolution) (fun y x ->
        (-halfX + float x * incX + incX / 2.0, halfY - float y * incY - incY / 2.0))
    {
        centre = ahead;
        i = frame.i;
        j = frame.j;
        resolution = resolution;
        pixelSize = (incX, incY)
        pixelCentres = offsets
    }

let generateRays camera resolution =
    let pixelGrid = createGrid camera resolution
    let rays = Seq.map (fun (x, y) ->
        let via = addP (addP pixelGrid.centre (mulV x pixelGrid.i)) (mulV y pixelGrid.j)
        { o = camera.o; d = subP via camera.o }) (pixelGrid.pixelCentres |> Seq.cast<float * float>)
    rays
