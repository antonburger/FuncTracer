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
                  // BUG: Have to ensure that light vectors are normalised beforehand.
                  (SoftDirectionalLight (Vector (1.0, -3.0, 3.0) |> normalise,8, System.Math.PI/5.0), Colour.white);
                  (SoftDirectionalLight (Vector (-3.0, -2.0, 3.0) |> normalise,8, System.Math.PI /5.0), Colour.white);
                  ];
    }

    let resolution = Resolution (400, 400)
    let c = { o = Point (0.0, 0.0, -20.0); up = Vector (0.0, 1.0, 0.0); lookAt = Point (0.0, 0.0, 1.0); fovY = System.Math.PI / 6.0; aspectRatio = 1.0 }
    let pixelRays = generateRays c resolution
    let shader = shadeOrBackground Colour.black lightShader 
    let pixels = shade shader scene pixelRays
    let b = { resolution = resolution; pixels = Seq.toList << Seq.map snd <| pixels }
    write b "test.ppm"
    0
