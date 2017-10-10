module Texture
open Colour

type TextureCoords = float*float
type Texture = TextureCoords -> Colour
let grid colour1 colour2 uv = 
    match uv with 
        | u,v when (u<0.5 && v<0.5) -> colour1
        | u,v when (u<0.5) -> colour2
        | u,v when (u>0.5 && v>0.5) -> colour1
        | _ -> colour2
