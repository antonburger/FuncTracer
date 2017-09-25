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
    let k = c.lookAt - c.o |> normalise
    let i = c.up .** k |> normalise
    let j = k .** i
    { i = i; j = j; k = k }

type ImagePlane = {
    origin: Point;
    originToCentre: Vector;
    i: Vector;
    j: Vector;
    resolution: Resolution;
    pixelSize: float * float;
    topLeft: float * float;
}

let createPlane camera resolution =
    let frame = fromCamera camera
    let ahead = camera.o + frame.k
    let halfY = System.Math.Tan(camera.fovY / 2.0)
    let halfX = halfY * camera.aspectRatio
    let incY = (halfY * 2.0) / (resH resolution - 1 |> float)
    let incX = (halfX * 2.0) / (resV resolution - 1 |> float)
    {
        origin = camera.o;
        originToCentre = frame.k;
        i = frame.i;
        j = frame.j;
        resolution = resolution;
        pixelSize = (incX, incY)
        topLeft = (-halfX + incX / 2.0, halfY - incY / 2.0)
    }

let rayThroughPixel imagePlane (pixel : int * int) (jitter : float * float) =
    let (cx, cy) = (fst imagePlane.topLeft + float (fst pixel) * fst imagePlane.pixelSize, snd imagePlane.topLeft - float (snd pixel) * snd imagePlane.pixelSize)
    let (jx, jy) = (cx + fst jitter * fst imagePlane.pixelSize, cy + snd jitter * snd imagePlane.pixelSize)
    let via = imagePlane.originToCentre + jx * imagePlane.i + jy * imagePlane.j
    { o = imagePlane.origin; d = via }

let generateRays camera resolution =
    let imagePlane = createPlane camera resolution
    let coords = seq { for y in 0..(resV resolution - 1) do for x in 0..(resH resolution - 1) -> (x, y) }
    // let getRays coord = List.singleton <| rayThroughPixel imagePlane coord
    let random = System.Random()
    let getRays coord =
        let jitters = List.init 16 (fun _ -> (random.NextDouble() - 0.5, random.NextDouble() - 0.5))
        List.map (fun jitter -> rayThroughPixel imagePlane coord jitter) jitters
    Seq.map (fun coord -> (coord, getRays coord)) coords
