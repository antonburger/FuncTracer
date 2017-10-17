module Tests.Parser.Colour
open System
open Xunit
open SceneParser
open FParsec

let result x = 
    match x with
    | Success (result, _, _) ->result
    | _ -> raise (Exception "Parsing failed")

let shouldFail x = 
    match x with
    | Failure _ -> ()
    | Success _ -> raise (Exception "Parsing should have failed")

[<Fact>]
let ``Triple of float interpreted as (r,g,b)`` () =
    let colour = run Parsers.pcolour "(1,0,0)" |> result
    Assert.Equal(Colour.red, colour)

[<Fact>]
let ``A single float specifies a grey value`` () =
    let colour = run Parsers.pcolour "1" |> result
    Assert.Equal(Colour.white, colour)

[<Fact>]
let ``Hex Notation`` ()=
    let colour = run Parsers.pcolour "#ff0000" |> result
    Assert.Equal(Colour.red, colour)


