// Learn more about F# at http://fsharp.org

open System
open Microsoft.FSharp.Control
open Ray
open Vector
open Sphere
open Plane
open Image
open Scene

let normalToColour = function
    | Some { n = Vector (x,y,z) } -> let remap c = (c + 1.0) / 2.0 * 255.0 |> byte in Colour (remap x, remap y, remap z)
    | _ -> Colour (0uy, 0uy, 0uy)

let shade scene (pixelRays : seq<(int * int) * Ray list>) =
    let shadePixel (rays : Ray list) =
        let intersections = Seq.map (fun ray -> intersectScene scene ray) rays
        // let intersections = Seq.map (fun ray -> async { return intersectScene scene ray }) rays |> Async.Parallel |> Async.RunSynchronously
        let colours = Seq.map normalToColour intersections
        let (fr, fg, fb) = Seq.fold (fun (ar, ag, ab) (Colour (r, g, b)) -> (ar + float r, ag + float g, ab + float b)) (0.0, 0.0, 0.0) colours
        let c = float rays.Length
        Colour (fr / c |> byte, fg / c |> byte, fb / c |> byte)
    Seq.map (fun pixelRay -> async { return (fst pixelRay, shadePixel <| snd pixelRay) }) pixelRays |> Async.Parallel |> Async.RunSynchronously
    // Seq.map (fun pixelRay -> (fst pixelRay, shadePixel <| snd pixelRay)) pixelRays

[<EntryPoint>]
let main argv =
    let s = Scene [
                  sphere (Point (0.0,0.0,10.0)) 4.0;
                  sphere (Point (1.0,3.0,10.0)) 4.0;
                //   plane (Point (0.0,0.0,0.0)) (Vector (0.0,1.0,0.0))
                  ]

    let resolution = Resolution (400, 400)
    let c = { o = Point (0.0, 0.0, 0.0); up = Vector (0.0, 1.0, 0.0); lookAt = Point (0.0, 0.0, 1.0); fovY = System.Math.PI / 2.0; aspectRatio = 1.0 }
    let pixelRays = generateRays c resolution
    let pixels = shade s pixelRays
    let b = { resolution = resolution; pixels = Seq.toList << Seq.map snd <| pixels }
    write b "test.ppm"
    0
