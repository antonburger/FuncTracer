module Shading
open Ray;
open Image;
open Vector;
open Scene;

let black = Colour (0uy, 0uy, 0uy)
let white = Colour (255uy, 255uy, 255uy)
let red = Colour (255uy, 0uy, 0uy)
let blue = Colour (0uy, 0uy, 255uy)

let shadeOrBackground background shader = function
    | Some fragment -> shader fragment
    | _ -> background


let normalToColourShader intersectedObject = 
    let normal = intersectedObject.intersection.n
    match normal with 
        | Vector (x,y,z) -> 
            let remap c = (c + 1.0) / 2.0 * 255.0 |> byte in Colour (remap x, remap y, remap z)


let directionalLightShader intersectedObject =
    let normal = intersectedObject.intersection.n
    let colour = intersectedObject.sceneObject.Material.colour;
    let lightDirection = -Vector(1.0, -1.0, 3.0) |> normalise
    scaleColour (lightDirection .* normal) colour

