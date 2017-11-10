module SceneParser

open System.IO
open FParsec.CharParsers
open FParsec.Primitives
open Vector
open Sphere
open Cylinder
open Cone
open Plane
open Light
open Image
open Scene
open Transform
open Csg
open Ray
open System.IO

module Parsers =
    open Cube

    let pipe6 p1 p2 p3 p4 p5 p6 f = 
        pipe5 p1 p2 p3 p4 (tuple2 p5 p6)
              (fun x1 x2 x3 x4 (x5, x6) -> f x1 x2 x3 x4 x5 x6)

    let private ws = skipMany (skipAnyOf [| ' '; '\t' |] <??> "space or tab")
    let private ws1 = skipMany1 (skipAnyOf [| ' '; '\t' |] <??> "space or tab")

    let private anyWhitespace: Parser<unit,unit> = 
        skipMany ((skipAnyOf [| ' '; '\t' |]) <|> skipNewline) 

    let private skipComment = skipChar ';' >>. skipRestOfLine true <?> "comment"
    let private skipTrivia = skipNewline <|> skipComment
    let private skipTrailingTrivia1 = skipMany1 skipTrivia

    let inBrackets (x:Parser<_, _>) = between (skipChar '(' >>. anyWhitespace) (anyWhitespace >>. skipChar ')') x

    let defaultTo a parser = opt parser |>> (fun v->Option.defaultValue a v)

    let private numberOptions =
        NumberLiteralOptions.AllowFraction |||
        NumberLiteralOptions.AllowMinusSign |||
        NumberLiteralOptions.AllowExponent

    let private nonNegativeNumberOptions =
        NumberLiteralOptions.AllowFraction |||
        NumberLiteralOptions.AllowExponent

    let private pnumber =
        numberLiteral numberOptions "number" |>> fun nl -> float nl.String

    let pint =
        numberLiteral NumberLiteralOptions.None "integer" |>> fun nl -> int nl.String
    let private pangle =
        pnumber |>> (fun v->Deg.toRad (v*1.0<deg>))

    let private pnonNegativeNumber =
        numberLiteral nonNegativeNumberOptions "non-negative number" |>> fun nl -> float nl.String

    let pkeyword name pValue =
        skipStringCI name >>. anyWhitespace >>. pValue

    let ptriple =
        let firstNumber = pnumber .>> ws
        let nextNumber = pchar ',' >>. ws >>. pnumber .>> ws
        let numberList = tuple3 firstNumber nextNumber nextNumber
        inBrackets numberList
        <??> "comma-separated list of 3 numbers in parens"

    let ppair =
        let firstNumber = pnumber .>> ws
        let nextNumber = pchar ',' >>. ws >>. pnumber .>> ws
        let numberList = tuple2 firstNumber nextNumber 
        inBrackets numberList
        <??> "comma-separated list of 2 numbers in parens"

    let private phexColour:Parser<Colour,unit> = 
        let hexToByte (hex: char[]) = System.Byte.Parse(new System.String(hex),System.Globalization.NumberStyles.AllowHexSpecifier)
        skipChar '#' >>. (manyMinMaxSatisfy 6 6 (fun (c:char)->true)
            |>> (fun (str:string)->
                    let arr = 
                        str |> Seq.windowed 2 
                        |> Seq.mapi (fun i x -> (i,x))
                        |> Seq.filter (fun (i,_)->i%2=0)
                        |> Seq.map snd
                        |> Seq.map hexToByte
                        |> Seq.map (fun v->float(v)/255.0)
                        |> Seq.toArray
                    Colour (arr.[0],arr.[1], arr.[2])
            )
        )

    let pcolour = ((ptriple) |>> (fun (r,g,b) -> Colour (r,g,b)))
                 <|> (pnumber |>> (fun x->Colour(x,x,x)))
                 <|> phexColour

    let pfile:Parser<string,unit> = 
        let normalChar = satisfy (fun c -> c <> '"')
        between (pstring "\"") (pstring "\"") (manyChars normalChar)

    let applied<'a, 'b> (f:Parser<'a->'b,unit>) argument =
        pipe2 
            (f .>> anyWhitespace) 
            argument
            (fun a b -> a b)

    let pmaterial = 
        let factory colour roughness reflectance shineyness = 
            { mattWhite with colour=        colour; 
                             reflectance=   reflectance; 
                             shineyness=    shineyness;
                             roughness=     roughness 
                             }
        pipe4
            (defaultTo Colour.white (pkeyword "diffuse" (pcolour .>> ws1)))
            (defaultTo 0.0 (pkeyword "roughness" (pfloat .>> ws1)))
            (defaultTo 0.0 (pkeyword "reflectance" (pfloat .>> ws1)))
            (defaultTo 0.0 (pkeyword "shineyness" pfloat))
            factory

    let geometry, geometryRef = createParserForwardedToRef<Geometry, unit>()

    let namedPrimitive name value = skipStringCI name |>> (fun()->value)

    let mesh =          
        let factory (file:string) = 
            // TODO: Move this out of the parser?
            match PlyParser.parse (new StreamReader(file):>TextReader) with 
            | Result.Ok triangles -> 
                printfn "Loaded %i triangles from %s" (triangles |> Seq.length) file
                triangles |> Seq.map (Triangle.triangle) |> group
            | Result.Error message -> 
                raise (System.Exception message)
        let arguments = pfile |>> factory
        pkeyword "mesh" arguments

    let bspMesh =          
        let factory depth (file:string) = 
            // TODO: Move this out of the parser?
            match PlyParser.parse (new StreamReader(file):>TextReader) with 
            | Result.Ok triangles -> 
                printfn "Loaded %i triangles from %s" (triangles |> Seq.length) file
                BspMesh.bspMesh false depth triangles 
            | Result.Error message -> 
                raise (System.Exception message)
        let arguments = pipe2 
                            (pint .>> ws1)
                            pfile 
                            factory
        pkeyword "bspMesh" arguments

    let primitive =
                    mesh <|> bspMesh <|>
                    namedPrimitive "circle" circle <|>
                    namedPrimitive "square" square <|>
                    namedPrimitive "cube" cube <|>
                    namedPrimitive "sphere" sphere <|>
                    namedPrimitive "plane" plane <|>
                    namedPrimitive "cone" cone <|>
                    namedPrimitive "solidCylinder" solidCylinder <|>
                    namedPrimitive "cylinder" cylinder 

    let hueShiftFunction:Parser<Geometry->Geometry, unit> = 
        pkeyword "hueShift" (pfloat |>> hueShift)

    let texture, textureRef = createParserForwardedToRef<Texture.Texture, unit>()

    let gridTexture: Parser<Texture.Texture, unit> =
        let arguments = 
            pipe2
                (pcolour .>> ws1)
                pcolour 
                Texture.grid
        pkeyword "grid" arguments

    let imageTexture:Parser<Texture.Texture,unit> = 
        let arguments = pfile |>> ImageTexture.image
        pkeyword "image" arguments

    let scaleTexture:Parser<Texture.Texture->Texture.Texture,unit> = 
        pkeyword "scale" (ppair |>> Texture.scale)

    let rotateTexture:Parser<Texture.Texture->Texture.Texture,unit> = 
        pkeyword "rotate" (pangle |>> Texture.rotate)

    let textureFunction = scaleTexture <|> rotateTexture

    do textureRef :=  gridTexture <|> imageTexture <|> inBrackets (applied textureFunction texture)

    let textureGeometryFunction: Parser<Geometry->Geometry, unit> = 
        pkeyword "texture" (texture |>> textureDiffuse)

    let scalefloat = 
        pnumber |>> (fun x->(x,x,x))

    let scaleFunction = 
        let factory (x,y,z) = transform (scale (x,y,z)) 
        let arguments = 
                ((ptriple <|> scalefloat) .>> ws1) |>> factory
        pkeyword "scale" arguments

    let materialFunction:Parser<Geometry->Geometry, unit> = 
        pkeyword "material" (pmaterial |>> setMaterial)

    let rotateFunction = 
        let factory (x,y,z) angle =
            let rotation = rotate (Vector (x,y,z)) (Deg.toRad (angle*1.0<deg>))
            transform rotation 
        let arguments = 
            pipe2
                (ptriple .>> ws1)
                pfloat
                factory
        pkeyword "rotate" arguments

    let (translateFunction:Parser<Geometry->Geometry, unit>) = 
        let factory (x,y,z) = Transform.transform (translate (Vector (x,y,z)))
        let arguments = 
                ptriple |>> factory
        pkeyword "translate" arguments

    let binaryGeometryFunction keyword f = 
        let factory object1 object2=
            f object1 object2 
        let objects =
            pipe2
                (geometry .>> ws1)
                geometry
                factory
        pkeyword keyword objects 

    let groupFunction  = 
        let argument = many (geometry .>> anyWhitespace)
        let factory arguments =
            group (List.toSeq arguments)
        let objects = argument |>> factory
        pkeyword "group" objects 

    let geometryFunction, geometryFunctionRef = createParserForwardedToRef<Geometry->Geometry, unit>()

    let composed f =  // BUG: Doesn't work without the brackets 
        pipe2 
            ((inBrackets f) .>> anyWhitespace .>> pchar '.' .>> anyWhitespace)
            (inBrackets f)
            (>>)

    let repeatFunction = 
        let factory count f = repeat count f
        let arguments = 
            pipe2
                (pint32 .>> anyWhitespace)
                (geometryFunction .>> anyWhitespace)
                factory
        pkeyword "repeat" arguments

    let ignoreLightFunction = skipStringCI "IgnoreLight" |>> (fun ()->ignoreLight)

    let (appliedFunction:Parser<Geometry,unit>) =
        binaryGeometryFunction "union"     Csg.union         <|>
        binaryGeometryFunction "subtract"  Csg.subtract      <|>
        binaryGeometryFunction "intersect" Csg.intersect     <|>
        binaryGeometryFunction "exclude"   Csg.exclude       <|>
        groupFunction<|>
        (applied geometryFunction geometry)

    do geometryFunctionRef := ignoreLightFunction <|> textureGeometryFunction <|> hueShiftFunction <|> materialFunction <|> repeatFunction <|> scaleFunction <|> translateFunction <|> rotateFunction <|> (composed geometryFunction)
    do geometryRef :=  primitive <|> inBrackets appliedFunction 

    let pobject = 
        (geometry .>> ws) |>> id

    let pobjects =
        sepEndBy pobject skipTrailingTrivia1

    let pcamera =
        let factory pos lookAt up fov ratio (maybeFocus:(float*float) option) =
            let focus = 
                maybeFocus 
                |> Option.map (fun (length, size)->{ focalLength=length; apetureAngularSize=Deg.toRad (size*1.0<deg>)}) 
            let newCamera = { 
                    o = Point pos; 
                    lookAt = Point lookAt; 
                    up = Vector up |> normalise; 
                    fovY = Deg.toRad (1.0<deg> * fov); aspectRatio = ratio 
                    focus = focus
                }
            fun options -> { options with camera = newCamera }
        let camera =
            pipe6
                (pkeyword "pos" (ptriple .>> ws1))
                (pkeyword "lookat" (ptriple .>> ws1))
                (pkeyword "up" (ptriple .>> ws1))
                (pkeyword "fov" (pnonNegativeNumber .>> ws1))
                (pkeyword "ratio" pnonNegativeNumber.>>ws)
                (opt (pkeyword "focus" ppair))
                factory
        pkeyword "camera" camera

    // samples ["corner"|number]
    // If "corner", then use corner sampling.
    // If <number>, then use random jittering with the specified number of samples.
    let psamples =
        let jitterFactory multisampleCount =
            fun options -> { options with samplingStrategy = JitteredSampling.strategy multisampleCount }
        let cornerFactory =
            fun options -> { options with samplingStrategy = CornerSampling.strategy }
        let jitterStrategy = pint32 |>> jitterFactory <??> "positive number"
        let cornerStrategy = pstring "corner" >>. preturn cornerFactory
        pkeyword "samples" (jitterStrategy <|> cornerStrategy)

    let presolution =
        let factory res =
            fun options -> { options with resolution = Resolution res } : SceneOptions
        let resolution =
            tuple2 (pint32 .>> ws1) pint32 |>> factory <??> "two positive numbers"
        pkeyword "res" resolution

    let poptions =
        let option = (pcamera <|> psamples <|> presolution) .>> ws
        sepEndBy option skipTrailingTrivia1

    let pdirectional =
        let factory dir colour = directional (Vector dir) colour
        let directional =
            pipe2
                (pkeyword "dir" (ptriple .>> ws1))
                (pkeyword "colour" pcolour)
                factory
        pkeyword "directional" directional

    let psoftDirectional =
        let factory dir samples scattering colour = softDirectional (Vector dir) samples (Deg.toRad 1.0<deg> * scattering) (Colour colour)
        let softDirectional =
            pipe4
                (pkeyword "dir" (ptriple .>> ws1))
                (pkeyword "samples" (pint32 .>> ws1))
                (pkeyword "scatter" (pnonNegativeNumber .>> ws1))
                (pkeyword "colour" ptriple)
                factory
        pkeyword "softdirectional" softDirectional

    let ppositional =
        let factory pos falloff colour = positional (Point pos) (Falloff falloff) (Colour colour)
        let positional =
            pipe3
                (pkeyword "pos" (ptriple .>> ws1))
                (pkeyword "falloff" (ptriple .>> ws1))
                (pkeyword "colour" ptriple)
                factory
        pkeyword "positional" positional

    let plights =
        let light = (pdirectional <|> psoftDirectional <|> ppositional) .>> ws
        sepEndBy light skipTrailingTrivia1

    let pscenegraph =
        let factory options objects lights = (options, { objects = objects; lights = lights })
        let skipLeadingTrivia = skipMany skipTrivia
        // BUG: within options, objects or lights, separate objects must be (correctly) newline-separated. But between sections, no newline is required - can run a line on straight from "camera" to "sphere", for instance. Would like to correct this in a way that doesn't *require* newline at EOF.
        let content = pipe3 poptions pobjects plights factory
        between skipLeadingTrivia eof content

let parse (reader : TextReader) =
    let result = run Parsers.pscenegraph (reader.ReadToEnd())
    match result with
    | Success ((options, scene), _, _) ->
        let finalOptions = Seq.fold (fun options setter -> setter options) SceneOptions.Default options
        FSharp.Core.Result.Ok (finalOptions, scene)
    | Failure (errorMsg, _, _) -> FSharp.Core.Result.Error errorMsg
