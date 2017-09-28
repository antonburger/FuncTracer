module Image

open System.IO
open Ray
open Vector

type Camera = { o: Point; lookAt: Point; up: Vector; fovY: float<rad>; aspectRatio: float }

let clamp x = 
        match x with
            | _ when x > 1.0 -> 1.0
            | _ when x < 0.0 -> 0.0
            | _ -> x
let scaleColour intensity =  
    Colour.map (fun c -> intensity * c ) 

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
            let toByte c = clamp c * 255.0 |> byte
            writer.WriteLine("{0} {1} {2}", toByte r, toByte g, toByte b)
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
    let height = tan (camera.fovY / 2.0<rad>) * 2.0
    let width = height * camera.aspectRatio
    let pixelHeight = height / (resH resolution - 1 |> float)
    let pixelWidth = width / (resV resolution - 1 |> float)
    {
        origin = camera.o;
        originToCentre = frame.k;
        i = frame.i;
        j = frame.j;
        resolution = resolution;
        pixelSize = (pixelWidth, pixelHeight)
        topLeft = (-width / 2.0 + pixelWidth / 2.0, height / 2.0 - pixelHeight / 2.0)
    }

let rayThroughPixel imagePlane (pixel : int * int) (jitter : float * float) =
    let (cx, cy) = (fst imagePlane.topLeft + float (fst pixel) * fst imagePlane.pixelSize, snd imagePlane.topLeft - float (snd pixel) * snd imagePlane.pixelSize)
    let (jx, jy) = (cx + fst jitter * fst imagePlane.pixelSize, cy + snd jitter * snd imagePlane.pixelSize)
    let via = imagePlane.originToCentre + jx * imagePlane.i + jy * imagePlane.j
    { o = imagePlane.origin; d = via }

let generateRays camera samplesPerPixel resolution =
    let imagePlane = createPlane camera resolution
    let coords = seq { for y in 0..(resV resolution - 1) do for x in 0..(resH resolution - 1) -> (x, y) }
    // let getRays coord = List.singleton <| rayThroughPixel imagePlane coord
    let random = System.Random()
    let getRays coord =
        let jitters = List.init samplesPerPixel (fun _ -> (random.NextDouble() - 0.5, random.NextDouble() - 0.5))
        List.map (fun jitter -> rayThroughPixel imagePlane coord jitter) jitters
    Seq.map (fun coord -> (coord, getRays coord)) coords
