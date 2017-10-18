module PlyParser

open System.IO
open FParsec.CharParsers
open FParsec.Primitives
open Triangle

type Header = {
    vertexCount: int;
    faceCount: int;
}


let private pint =
    numberLiteral NumberLiteralOptions.None "integer" |>> fun nl -> int nl.String
let private pfloat =
    let numberOptions = NumberLiteralOptions.AllowMinusSign ||| NumberLiteralOptions.AllowFraction
    numberLiteral numberOptions "float" |>> fun nl -> float nl.String

let magicNumber = skipString "ply" .>> skipNewline

let ignoreHeader prefix = 
    skipString prefix .>> skipRestOfLine true

let ignoredHeaderLines = 
    ignoreHeader "format" <|> ignoreHeader "comment" <|> ignoreHeader "property"

let vertexCount = skipString "element vertex " >>. pint 
let faceCount = skipString "element face " >>. pint 

let pheader = 
    magicNumber 
    >>. many ignoredHeaderLines 
    >>. pipe2
        (vertexCount .>> skipNewline)
        (many ignoredHeaderLines >>. faceCount .>> skipNewline)
        (fun vc fc -> {vertexCount = vc; faceCount=fc})
    .>> (many ignoredHeaderLines) 
    .>> skipString "end_header" 
    .>> skipNewline

let pvertex = 
    pipe5
        (pfloat .>> (skipChar ' '))
        (pfloat .>> (skipChar ' '))
        (pfloat .>> (skipChar ' '))
        (pfloat .>> (skipChar ' '))
        (pfloat .>> skipRestOfLine true)
        (fun x y z confidence intensity -> Point(x,y,z))

let pface (vertexes:Point[]) = 
    skipString "3 " >>.
    pipe3
       (pint .>> (skipChar ' '))
       (pint .>> (skipChar ' '))
       (pint .>> skipRestOfLine true)
       (fun a b c -> triangle vertexes.[a] vertexes.[b] vertexes.[c])


let pbody header = 
    let pfaces vertexes = parray header.faceCount (pface vertexes)
    (parray header.vertexCount pvertex) >>= pfaces

let plyFile = 
    pheader >>= pbody

let parse (reader : TextReader) =
    let result = run plyFile (reader.ReadToEnd())
    match result with
    | Success (triangles, _, _) -> Result.Ok triangles
    | Failure (errorMsg, _, _) -> Result.Error errorMsg