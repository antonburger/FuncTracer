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

let generateRays camera resolution =
    // Camera's a left-handed coordinate system (+x right, +y up, +z forward). So right = up x forward rather than forward x up.
    let forward = subP camera.lookAt camera.o |> normalise
    let right = cross camera.up forward |> normalise
    let up = cross forward right |> normalise
    let ahead = addP camera.o forward
    let halfY = System.Math.Tan(camera.fovY / 2.0)
    let halfX = halfY * camera.aspectRatio
    let incY = (halfY * 2.0) / (resH resolution - 1 |> float)
    let incX = (halfX * 2.0) / (resV resolution - 1 |> float)
    let offsets = Seq.unfold (fun (x, y) ->
        let makeOffsets = (-halfX + float x * incX + incX / 2.0, -halfY + float y * incY + incY / 2.0)
        let makeState = if x + 1 < resH resolution then (x + 1, y) else (0, y + 1)
        if y < resV resolution
            then Some (makeOffsets, makeState)
            else None) (0, 0)
    let rays = Seq.map (fun (x, y) ->
        let via = addP (addP ahead (mulV x right)) (mulV -y up)
        { o = camera.o; d = subP via camera.o }) offsets
    rays
