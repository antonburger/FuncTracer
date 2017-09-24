module Shading
open Ray;
open Image;
open Vector;

let black = Colour (0uy, 0uy, 0uy)
let white = Colour (255uy, 255uy, 255uy)

let shadeOrBackground background shader = function
    | Some fragment -> shader fragment
    | _ -> background

let clamp x = 
        match x with
            | _ when x > 255.0 -> 255uy
            | _ when x<0.0     -> 0uy
            | _                -> (byte) x

let normalToColourShader { n = Vector (x,y,z) } = 
    let remap c = (c + 1.0) / 2.0 * 255.0 |> byte in Colour (remap x, remap y, remap z)

let directionalLightShader { n = normal } =
    let lightDirection = Vector(1.0, -1.0, 3.0) |> negV |> normalise
    let intensity = clamp (dot lightDirection normal * 255.0)
    Colour(intensity, intensity, intensity);

