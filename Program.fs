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
                let resolution = Resolution (400, 400)
                let pixelRays = generateRays options.camera options.multisampleCount resolution
                let shader = multiPartShader [reflectionShader; diffuseShader]
                let pixels = shade shader scene pixelRays
                let b = { resolution = resolution; pixels = Seq.toList << Seq.map snd <| pixels }
                write b "test.ppm" 
                0
             )
