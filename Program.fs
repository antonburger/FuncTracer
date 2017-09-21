// Learn more about F# at http://fsharp.org

open System
open Ray
open Vector
open Sphere
open Plane
open Image
open Scene

let normalToColour = function
    | Some { n = Vector (x,y,z) } -> let remap c = (c + 1.0) / 2.0 * 255.0 |> byte in Colour (remap x, remap y, remap z)
    | _ -> Colour (0uy, 0uy, 0uy)

[<EntryPoint>]
let main argv =
    let s = Scene [
                  sphere (Point (0.0,0.0,10.0)) 4.0;
                  plane (Point (0.0,0.0,0.0)) (Vector (0.0,1.0,0.0))
                  ]

    let resolution = Resolution (400, 400)
    let c = { o = Point (0.0, 1.0, 0.0); up = Vector (0.0, 1.0, 0.0); lookAt = Point (0.0, 0.0, 1.0); fovY = System.Math.PI / 2.0; aspectRatio = 1.0 }
    let rays = generateRays c resolution
    let intersections = rays |> Seq.map (intersectScene s)
    let pixels = Seq.map normalToColour intersections
    let b = { resolution = resolution; pixels = Seq.toList pixels }
    write b "test.ppm"
    0
