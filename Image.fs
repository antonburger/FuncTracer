module Image

open System.IO
open Ray
open Vector
open Jitter
open SixLabors.ImageSharp

type Focus = { focalLength:float; apetureAngularSize:float<rad>}
type Camera = { 
                o: Point; 
                lookAt: Point; 
                up: Vector; 
                fovY: float<rad>; 
                aspectRatio: float;
                focus:Focus option
              }
type PixelCoord = PixelCoord of x: int * y: int

type SamplingStrategy = {
    generateRays: unit -> Ray list;
    blendPixels: Colour seq -> (PixelCoord * Colour) list
}

let scaleColour intensity =  
    Colour.map (fun c -> intensity * c ) 

type Resolution = Resolution of (int * int)

type Bitmap = { resolution: Resolution; pixels : Colour list }

let resH (Resolution r) = fst r
let resV (Resolution r) = snd r

let write bitmap output =
    let toByte c = Math.clamp c * 255.0 |> byte
    let width = resH bitmap.resolution
    let writePixel (image: Image<Rgba32>) (index: int) (Colour (r, g, b)) =
        let x, y = index % width, index / width
        image.[x, y] <- Rgba32(toByte r, toByte g, toByte b)
    use image = new Image<Rgba32>(resH bitmap.resolution, resV bitmap.resolution)
    //use output = new FileStream(filename, FileMode.Create, FileAccess.Write, FileShare.None)
    List.iteri (writePixel image) bitmap.pixels
    image.Save(output, Formats.Png.PngEncoder())

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

// General tools for working with image planes.
module ImagePlane =
    let create camera resolution =
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

    let rayThroughPixel imagePlane (PixelCoord (px, py)) (JitterOffset (jitterX, jitterY)) =
        let (originX, originY) = imagePlane.topLeft
        let (pwidth, pheight) = imagePlane.pixelSize
        let (centreX, centreY) = (originX + float px * pwidth, originY - float py * pheight)
        let (jx, jy) = (centreX + jitterX * pwidth, centreY + jitterY * pheight)
        let via = imagePlane.originToCentre + jx * imagePlane.i + jy * imagePlane.j
        { o = imagePlane.origin; d = via }

    let depthOfFieldJitter focus =
        shiftOrigin focus.focalLength
        >> Ray.jitterDirection focus.apetureAngularSize
        >> shiftOrigin -focus.focalLength

// Multisampled jittered strategy.
module JitteredSampling =
    open ImagePlane

    let private generateRays samplesPerPixel imagePlane =
        let random = System.Random()
        let jitterPattern sampleCount = Jitter.pattern random Jitter.circle sampleCount
        fun () ->
            let pixels = seq { for y in 0..(resV imagePlane.resolution - 1) do for x in 0..(resH imagePlane.resolution - 1) -> PixelCoord (x, y) }
            let jitterPatterns = let pattern = jitterPattern samplesPerPixel in Seq.initInfinite (fun _ -> pattern)
            let pixelRays (pixel, jitterPattern) =
                // Given a jitter offset, generate a ray with the given offset through the specified pixel on the image plane.
                let jitteredRay = rayThroughPixel imagePlane pixel
                List.map jitteredRay jitterPattern
            Seq.collect pixelRays <| Seq.zip pixels jitterPatterns |> Seq.toList

    let private blendPixels samplesPerPixel resolution =
        fun (colours: Colour seq) ->
            let pixels = seq { for y in 0..(resV resolution - 1) do for x in 0..(resH resolution - 1) -> PixelCoord (x, y) }
            let c = colours |> Seq.chunkBySize samplesPerPixel |> Seq.map Array.average
            Seq.zip pixels c |> Seq.toList

    let strategy samplesPerPixel imagePlane =
        {
            generateRays = generateRays samplesPerPixel imagePlane;
            blendPixels = blendPixels samplesPerPixel imagePlane.resolution;
        }

// Corner-sampling strategy.
module CornerSampling =
    open ImagePlane

    let private generateRays imagePlane =
        fun () ->
            let pixels = seq { for y in 0..(resV imagePlane.resolution) do for x in 0..(resH imagePlane.resolution) -> PixelCoord (x, y) }
            let jitterOffset = JitterOffset (-0.5, 0.5)
            Seq.map (fun pixel -> rayThroughPixel imagePlane pixel jitterOffset) pixels |> Seq.toList

    let private blendPixels resolution =
        fun (colours: Colour seq) ->
            let colours2 = Seq.toArray colours
            let stride = resH resolution + 1
            let colourForPixel (PixelCoord (x, y)) =
                let corners = [y * stride + x; y * stride + x + 1; (y + 1) * stride + x; (y + 1) * stride + x + 1]
                let cornerColours = Seq.map (Array.get colours2) corners
                Seq.average cornerColours
            let pixels = seq { for y in 0..(resV resolution - 1) do for x in 0..(resH resolution - 1) -> PixelCoord (x, y) }
            let c = Seq.map colourForPixel pixels
            Seq.zip pixels c |> Seq.toList

    let strategy imagePlane =
        {
            generateRays = generateRays imagePlane;
            blendPixels = blendPixels imagePlane.resolution;
        }
