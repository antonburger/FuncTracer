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
    let returnValue = f(timer)
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

let runTracer (timer:Diagnostics.Stopwatch, input:TextReader, output:Stream) = 
    let (options, scene) = readScene input
    printfn "Parsed input %ims" timer.ElapsedMilliseconds
    let (pixelRays, count) = generateRays options.camera options.multisampleCount options.resolution
    printfn "Generated %i rays: %ims" count timer.ElapsedMilliseconds
    let shader = multiPartShader [specularShader; reflectionShader; diffuseShader]
    printfn "Created shader: %ims" timer.ElapsedMilliseconds
    let pixels = shade shader scene pixelRays
    printfn "Shaded scene %ims" timer.ElapsedMilliseconds
    let bitmap = { resolution = options.resolution; pixels = Seq.toList << Seq.map snd <| pixels }
    printfn "Writing output %ims" timer.ElapsedMilliseconds
    write bitmap output 
    0

let getOutputStream (args : string[]) : Stream = 
    match args.Length with
    | 2 -> 
        printfn "Using output file: %s" args.[1]
        new FileStream(args.[1] , FileMode.Create, FileAccess.Write, FileShare.None) :> Stream
    | 1 -> 
        printfn "Using standard output"
        Console.OpenStandardOutput() 
    | _ -> 
        printfn "Using default output file: test.png"        
        new FileStream("test.png" , FileMode.Create, FileAccess.Write, FileShare.None) :> Stream

let getInputStream (args : string[]) : TextReader =
    match args.Length with
    | 0 -> 
        printfn "Using default input file: sample.scene"
        file "sample.scene" :> TextReader
    | 1 -> 
        printfn "Using standard input"
        Console.In 
    | _ -> 
        printfn "Using input file: %s" args.[0]
        file args.[0] :> TextReader    

[<EntryPoint>]
let main (args) =
    use input = getInputStream args
    use output = getOutputStream args
    duration (fun timer -> runTracer (timer, input, output))
    
