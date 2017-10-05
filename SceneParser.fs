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

module Parsers =
    open Cube
    let private ws = skipMany (skipAnyOf [| ' '; '\t' |] <??> "space or tab")
    let private ws1 = skipMany1 (skipAnyOf [| ' '; '\t' |] <??> "space or tab")

    let private anyWhitespace: Parser<unit,unit> = 
        skipMany ((skipAnyOf [| ' '; '\t' |]) <|> skipNewline) 

    let private skipComment = skipChar '#' >>. skipRestOfLine true <?> "comment"
    let private skipTrivia = skipNewline <|> skipComment
    let private skipTrailingTrivia1 = skipMany1 skipTrivia

    let inBrackets (x:Parser<'a,'b>) = between (skipChar '(' >>. anyWhitespace) (anyWhitespace >>. skipChar ')') x

    let private numberOptions =
        NumberLiteralOptions.AllowFraction |||
        NumberLiteralOptions.AllowMinusSign |||
        NumberLiteralOptions.AllowExponent

    let private nonNegativeNumberOptions =
        NumberLiteralOptions.AllowFraction |||
        NumberLiteralOptions.AllowExponent

    let private pnumber =
        numberLiteral numberOptions "number" |>> fun nl -> float nl.String

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

    let pmaterial = 
        let factory (r,g,b) reflectance shineyness = 
            { colour=(Colour(r,g,b)); reflectance=reflectance; shineyness=shineyness}
        pipe3
            (pkeyword "diffuse" (ptriple .>> ws1))
            (pkeyword "reflectance" (pfloat .>> ws1))
            (pkeyword "shineyness" pfloat)
            factory

    let geometry, geometryRef = createParserForwardedToRef<Geometry, unit>()
    let psphere =
        let factory centre radius =
            // TODO: Allow arbitrary transforms. This was just the easiest way to prove the transforms without changing the file format :P
            let transform = compose [scale (radius, radius, radius); translate (Vector centre)]
            Transform.transform transform sphere
        let sphere =
            pipe2
                (pkeyword "pos" ptriple .>> ws1)
                (pkeyword "radius" pnonNegativeNumber)
                factory
        pkeyword "sphere" sphere

    let pcylinder =
        let factory centre radius height =
            // TODO: Allow arbitrary transforms. This was just the easiest way to prove the transforms without changing the file format :P
            let transform = compose [scale (radius, height, radius); translate (Vector centre)]
            Transform.transform transform cylinder
        let cylinder =
            pipe3
                (pkeyword "pos" (ptriple .>> ws1))
                (pkeyword "radius" (pnonNegativeNumber .>> ws1))
                (pkeyword "height" pnonNegativeNumber)
                factory
        pkeyword "cylinder" cylinder

    let psolidCylinder =
        let factory centre radius height =
            let transform = compose [scale (radius, height, radius); translate (Vector centre)]
            Transform.transform transform solidCylinder 
        let cylinder =
            pipe3
                (pkeyword "pos" (ptriple .>> ws1))
                (pkeyword "radius" (pnonNegativeNumber .>> ws1))
                (pkeyword "height" pnonNegativeNumber)
                factory
        pkeyword "solidCylinder" cylinder

    let pcone =
        let factory centre radius height =
            // TODO: Allow arbitrary transforms. This was just the easiest way to prove the transforms without changing the file format :P
            let transform = compose [scale (radius, height, radius); translate (Vector centre)]
            Transform.transform transform cone
        let cone =
            pipe3
                (pkeyword "pos" (ptriple .>> ws1))
                (pkeyword "radius" (pnonNegativeNumber .>> ws1))
                (pkeyword "height" (pnonNegativeNumber .>> ws1))
                factory
        pkeyword "cone" cone

    let pplane =
        let factory point normal = plane (Point point) (Vector normal |> normalise) 

        let plane =
            pipe2
                (pkeyword "point" (ptriple .>> ws1))
                (pkeyword "normal" (ptriple .>> ws1))
                factory
        pkeyword "plane" plane

    let namedPrimitive name value = skipStringCI name |>> (fun()->value)

    let primitive = psphere <|> pplane <|> pcylinder <|> pcone <|> psolidCylinder <|>
                    namedPrimitive "circle" circle <|>
                    namedPrimitive "square" square <|>
                    namedPrimitive "cube" cube 

    let scaleFunction = 
        let factory (x,y,z) = transform (scale (x,y,z)) 
        let arguments = 
                (ptriple .>> ws1) |>> factory
        pkeyword "scale" arguments

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


    let applied (f:Parser<Geometry->Geometry,unit>) =
        pipe2 
            (f .>> anyWhitespace) 
            geometry 
            (fun a (b:Geometry) -> a b)

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

    let composedFunction = 
        pipe2 
            ((inBrackets geometryFunction) .>> anyWhitespace .>> pchar '.' .>> anyWhitespace)
            (inBrackets geometryFunction)
            (>>)

    let repeatFunction = 
        let factory count f = repeat count f
        let arguments = 
            pipe2
                (pint32 .>> anyWhitespace)
                (geometryFunction .>> anyWhitespace)
                factory
        pkeyword "repeat" arguments



    let (appliedFunction:Parser<Geometry,unit>) =
        binaryGeometryFunction "union"     Csg.union         <|>
        binaryGeometryFunction "subtract"  Csg.subtract      <|>
        binaryGeometryFunction "intersect" Csg.intersect     <|>
        binaryGeometryFunction "exclude"   Csg.exclude       <|>
        groupFunction<|>
        (applied  geometryFunction)

    do geometryFunctionRef := repeatFunction <|> scaleFunction <|> translateFunction <|> rotateFunction <|> composedFunction
    do geometryRef :=  primitive <|> inBrackets appliedFunction 

    let pobject = 
        let factory geometry material = setMaterial material geometry
        pipe2
           (geometry .>> ws)
           pmaterial
           factory

    let pobjects =
        sepEndBy pobject skipTrailingTrivia1

    let pcamera =
        let factory pos lookAt up fov ratio =
            let newCamera = { o = Point pos; lookAt = Point lookAt; up = Vector up |> normalise; fovY = Deg.toRad (1.0<deg> * fov); aspectRatio = ratio }
            fun options -> { options with camera = newCamera }
        let camera =
            pipe5
                (pkeyword "pos" (ptriple .>> ws1))
                (pkeyword "lookat" (ptriple .>> ws1))
                (pkeyword "up" (ptriple .>> ws1))
                (pkeyword "fov" (pnonNegativeNumber .>> ws1))
                (pkeyword "ratio" pnonNegativeNumber)
                factory
        pkeyword "camera" camera

    let psamples =
        let factory multisampleCount =
            fun options -> { options with multisampleCount = multisampleCount }
        let samples = pint32 |>> factory <??> "positive number"
        pkeyword "samples" samples

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
        let factory dir colour = directional (Vector dir) (Colour colour)
        let directional =
            pipe2
                (pkeyword "dir" (ptriple .>> ws1))
                (pkeyword "colour" ptriple)
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
