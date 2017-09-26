open System
open Microsoft.FSharp.Control
open Ray
open Vector
open Sphere
open Plane
open Light
open Image
open Scene
open Shading

[<EntryPoint>]
let main argv =
    let scene = {
        objects = [
                  SceneObject(sphere (Point (-3.0,0.0,10.0)) 3.0, { colour = Colour.red });
                  SceneObject(sphere (Point (3.0,0.0,7.0)) 3.0, { colour = Colour.blue });
                  SceneObject(sphere (Point (1.0,3.0,4.0)) 1.0, { colour = Colour.greyScale 0.8 });
                  SceneObject(plane (Point (0.0,-3.0,0.0)) (Vector (0.0,1.0,0.0)), { colour = Colour.fromRGB 1 1 0 });
                  ];
        lights =  [
                  softDirectional (Vector (1.0, -3.0, 3.0)) 8 (System.Math.PI/5.0) Colour.white;
                  softDirectional (Vector (-3.0, -2.0, 3.0)) 8 (System.Math.PI /5.0) Colour.white;
                  ];
    }

    let resolution = Resolution (400, 400)
    let c = { o = Point (0.0, 0.0, -20.0); up = Vector (0.0, 1.0, 0.0); lookAt = Point (0.0, 0.0, 1.0); fovY = Deg.toRad 50.0<deg>; aspectRatio = 1.0 }
    let pixelRays = generateRays c resolution
    let pixels = shade lightShader scene pixelRays
    let b = { resolution = resolution; pixels = Seq.toList << Seq.map snd <| pixels }
    write b "test.ppm"
    0
