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
    eprintfn "Elapsed Time: %ims" timer.ElapsedMilliseconds
    returnValue    

//let addGeometry scene = 
//    let sphere1 = TransformedObject(Sphere(), compose [scale (3.0, 3.0, 3.0); translate (Vector(0.0,1.0,10.0) )])
//    let sphere2 = TransformedObject(Sphere(), compose [scale (3.0, 3.0, 3.0); translate (Vector(2.0,2.0,8.0) )])
//    let mattWhite obj = SceneObject( obj, { colour=Colour(1.0,1.0,1.0); reflectance= 0.0; shineyness= 0.0 })
//    scene |> Scene.addObject (mattWhite (Subtract(sphere1,sphere2) ))

let printIntersectionAt pixel =
    let (options, scene) = readScene <| file "sample.scene"
    let imagePlane = createPlane options.camera options.resolution
    let ray=rayThroughPixel imagePlane pixel (Jitter.JitterOffset (0.0, 0.0))
    let intersection = intersectScene scene ray
    eprintfn "intersection:"
    eprintfn "%A" intersection

let runTracer (timer:Diagnostics.Stopwatch, input:TextReader, output:Stream) = 
    let (options, scene) = readScene input
    eprintfn "Parsed input %ims" timer.ElapsedMilliseconds
    let pixelRays = generateRays options.camera options.multisampleCount options.resolution
    eprintfn "Generated rays: %ims" timer.ElapsedMilliseconds
    let shader = multiPartShader [specularShader; reflectionShader; diffuseShader]
    eprintfn "Created shader: %ims" timer.ElapsedMilliseconds
    let pixels = shade shader scene pixelRays
    eprintfn "Shaded scene %ims" timer.ElapsedMilliseconds
    let bitmap = { resolution = options.resolution; pixels = Seq.toList << Seq.map snd <| pixels }
    eprintfn "Writing output %ims" timer.ElapsedMilliseconds
    write bitmap output 
    0

let getOutputStream (args : string[]) : Stream = 
    match args.Length with
    // | 1 -> 
    //     eprintfn "Using default output file: test.png"        
    //     new FileStream("test.png" , FileMode.Create, FileAccess.Write, FileShare.None) :> Stream
    | 2 -> 
        eprintfn "Using output file: %s" args.[1]
        new FileStream(args.[1] , FileMode.Create, FileAccess.Write, FileShare.None) :> Stream
    | _ -> 
        eprintfn "Using standard output"
        Console.OpenStandardOutput() 
    
let getInputStream (args : string[]) : TextReader =
    match args.Length with
    | 0 -> 
        eprintfn "Using standard input"
        Console.In 
    | _ -> 
        eprintfn "Using input file: %s" args.[0]
        file args.[0] :> TextReader    

[<EntryPoint>]
let main (args) =
    eprintfn "Arguments: %s" args.[0]
    use input = getInputStream args
    use output = getOutputStream args
    duration (fun timer -> runTracer (timer, input, output))
    
