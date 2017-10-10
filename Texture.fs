module Texture
open Colour

type TextureCoords = float*float
type Texture<'a> = TextureCoords -> 'a

let grid value1 value2 uv = 
    match uv with 
        | u,v when (u<0.5 && v<0.5) -> value1
        | u,v when (u<0.5) -> value2
        | u,v when (u>0.5 && v>0.5) -> value1
        | _ -> value2
