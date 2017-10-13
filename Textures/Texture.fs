module Texture
open Colour
open Transform

type TextureCoords = float*float
type Texture = TextureCoords -> Colour

let repeat (u:float,v:float) =
    let repeatOne x = 
        let flipNegative a = if (a<0.0) then 1.0-a else a
        abs (x - floor x) |> flipNegative
    (repeatOne u, repeatOne v)

let scale scale (texture:Texture):Texture = 
    let scaleCoordinates (x,y) (u,v) = (u/x,v/y)
    scaleCoordinates scale >> texture

let rotate angle texture:Texture = 
    let rotateUV (u,v) = 
        let vectorToCoords (Vector (x,y,z))= (x,z)
        (matrix (rotate (Vector (0.0,1.0,0.0)) angle)) * (Vector (u,0.0,v)) |> vectorToCoords
    rotateUV >> texture

let grid colour1 colour2 uv = 
    match repeat uv with 
        | u,v when (u<0.5 && v<0.5) -> colour1
        | u,v when (u<0.5) -> colour2
        | u,v when (u>0.5 && v>0.5) -> colour1
        | _ -> colour2
