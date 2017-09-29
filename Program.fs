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
open Transform

let readScene =
    match SceneParser.parse Console.In with
    | Ok sceneAndOptions -> sceneAndOptions
    | Error message ->
        Console.WriteLine(message)
        Environment.Exit(1)
        failwith message

let duration f = 
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let returnValue = f()
    printfn "Elapsed Time: %ims" timer.ElapsedMilliseconds
    returnValue    

[<EntryPoint>]
let main argv =
    duration (fun () -> 
                let (options, scene) = readScene
                let pixelRays = generateRays options.camera options.multisampleCount options.resolution
                let shader = multiPartShader [specualarShader; reflectionShader; diffuseShader]
                let pixels = shade shader scene pixelRays
                let b = { resolution = options.resolution; pixels = Seq.toList << Seq.map snd <| pixels }
                write b "test.ppm" 
                0
             )
