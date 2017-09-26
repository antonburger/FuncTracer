open System
open Microsoft.FSharp.Control
open Ray
open Vector
open Sphere
open Plane
open Image
open Scene
open Shading


let shade shader scene (pixelRays : seq<(int * int) * Ray list>) =
    let shadePixel (rays : Ray list) =
        rays |>
        Seq.map (intersectScene scene >> shader) |>
        Seq.fold (+) Colour.black |>
        Colour.map (fun c -> c / float rays.Length)
    Seq.map (fun pixelRay -> async { return (fst pixelRay, shadePixel <| snd pixelRay) }) pixelRays |> Async.Parallel |> Async.RunSynchronously

[<EntryPoint>]
let main argv =
    let shader = shadeOrBackground Colour.white directionalLightShader
    let s = Scene [
                  SceneObject(sphere (Point (0.0,0.0,10.0)) 4.0, { colour = Colour.red });
                  SceneObject(sphere (Point (1.0,3.0,10.0)) 4.0, { colour = Colour.blue })
                //   plane (Point (0.0,0.0,0.0)) (Vector (0.0,1.0,0.0))
                  ]

    let resolution = Resolution (400, 400)
    let c = { o = Point (0.0, 0.0, 0.0); up = Vector (0.0, 1.0, 0.0); lookAt = Point (0.0, 0.0, 1.0); fovY = System.Math.PI / 2.0; aspectRatio = 1.0 }
    let pixelRays = generateRays c resolution
    let pixels = shade shader s pixelRays
    let b = { resolution = resolution; pixels = Seq.toList << Seq.map snd <| pixels }
    write b "test.ppm"
    0
