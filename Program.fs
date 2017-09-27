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

let readScene =
    match SceneParser.parse Console.In with
    | Ok scene -> scene
    | Error message ->
        Console.WriteLine(message)
        Environment.Exit(1)
        failwith message

[<EntryPoint>]
let main argv =
    let (options, scene) = readScene

    let resolution = Resolution (400, 400)
    let pixelRays = generateRays options.camera options.multisampleCount resolution
    let pixels = shade lightShader scene pixelRays
    let b = { resolution = resolution; pixels = Seq.toList << Seq.map snd <| pixels }
    write b "test.ppm"
    0
