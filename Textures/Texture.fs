module Texture
open Colour


let repeat (u:float,v:float) =
    let repeatOne x = 
        let flipNegative a = if (a<0.0) then 1.0-a else a
        abs (x - floor x) |> flipNegative
    (repeatOne u, repeatOne v)


type TextureCoords = float*float
type Texture = TextureCoords -> Colour
let grid colour1 colour2 uv = 
    match repeat uv with 
        | u,v when (u<0.5 && v<0.5) -> colour1
        | u,v when (u<0.5) -> colour2
        | u,v when (u>0.5 && v>0.5) -> colour1
        | _ -> colour2
