module SceneParser

open System.IO
open FParsec.CharParsers
open FParsec.Primitives
open Vector
open Sphere
open Plane
open Light
open Image
open Scene

module Parsers =
    let private ws = skipMany (skipAnyOf [| ' '; '\t' |])
    let private ws1 = skipMany1 (skipAnyOf [| ' '; '\t' |])
    let private newlines1 = skipMany1 skipNewline
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
        skipStringCI name >>. ws1 >>. pValue

    let ptriple =
        let firstNumber = pnumber .>> ws
        let nextNumber = pchar ',' >>. ws >>. pnumber .>> ws
        let numberList = tuple3 firstNumber nextNumber nextNumber
        between (skipChar '(' >>. ws) (skipChar ')') numberList

    let pmaterial = 
        let factory (r,g,b) reflectance = {colour=(Colour(r,g,b)); reflectance=reflectance}
        pipe2
            (pkeyword "diffuse" (ptriple .>> ws1))
            (pkeyword "reflectance" pfloat)
            factory

    let psphere =
        let factory centre radius material = SceneObject(Sphere(Point centre, radius), material)
        let sphere =
            pipe3
                (pkeyword "pos" (ptriple .>> ws1))
                (pkeyword "radius" (pnonNegativeNumber .>> ws1))
                pmaterial
                factory
        pkeyword "sphere" sphere

    let pplane =
        let factory point normal material = SceneObject(Plane(Point point, Vector normal |> normalise), material)
        let plane =
            pipe3
                (pkeyword "point" (ptriple .>> ws1))
                (pkeyword "normal" (ptriple .>> ws1))
                pmaterial
                factory
        pkeyword "plane" plane

    let pobjects =
        let object = (psphere <|> pplane) .>> ws
        sepEndBy object newlines1

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
        let samples = pint32 |>> factory
        pkeyword "samples" samples

    let poptions =
        let option = (pcamera <|> psamples) .>> ws
        sepEndBy option newlines1

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

    let plights =
        let light = (pdirectional <|> psoftDirectional) .>> ws
        sepEndBy light newlines1

    let pscenegraph =
        let factory options objects lights _ = (options, { objects = objects; lights = lights })
        pipe4 poptions pobjects plights eof factory

let parse (reader : TextReader) =
    let result = run Parsers.pscenegraph (reader.ReadToEnd())
    match result with
    | Success ((options, scene), _, _) ->
        let finalOptions = Seq.fold (fun options setter -> setter options) SceneOptions.Default options
        FSharp.Core.Result.Ok (finalOptions, scene)
    | Failure (errorMsg, _, _) -> FSharp.Core.Result.Error errorMsg
