open System
open System.IO
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

let readScene source =
    match SceneParser.parse source with
    | Ok sceneAndOptions -> sceneAndOptions
    | Error message ->
        Console.WriteLine(message)
        Environment.Exit(1)
        failwith message

let file (name:string) = new StreamReader(name)

let duration f = 
    let timer = System.Diagnostics.Stopwatch()
    timer.Start()
    let returnValue = f()
    printfn "Elapsed Time: %ims" timer.ElapsedMilliseconds
    returnValue    

//let addGeometry scene = 
//    let sphere1 = TransformedObject(Sphere(), compose [scale (3.0, 3.0, 3.0); translate (Vector(0.0,1.0,10.0) )])
//    let sphere2 = TransformedObject(Sphere(), compose [scale (3.0, 3.0, 3.0); translate (Vector(2.0,2.0,8.0) )])
//    let mattWhite obj = SceneObject( obj, { colour=Colour(1.0,1.0,1.0); reflectance= 0.0; shineyness= 0.0 })
//    scene |> Scene.addObject (mattWhite (Subtract(sphere1,sphere2) ))

let printIntersectionAt (x,y) =  
    let (options, scene) = readScene <| file "sample.scene"
    let imagePlane = createPlane options.camera options.resolution
    let ray=rayThroughPixel imagePlane (x,y) (0.0,0.0)
    let intersection = intersectScene scene ray
    printfn "intersection:"
    printfn "%A" intersection

[<EntryPoint>]
let main argv =
    duration (fun () -> 
                let (options, scene) = readScene Console.In
                let pixelRays = generateRays options.camera options.multisampleCount options.resolution
                let shader = multiPartShader [specualarShader; reflectionShader; diffuseShader]
                let pixels = shade shader scene pixelRays
                let bitmap = { resolution = options.resolution; pixels = Seq.toList << Seq.map snd <| pixels }
                write bitmap "test.png"
                0
             )
