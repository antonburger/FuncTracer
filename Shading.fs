module Shading
open Ray;
open Image;
open Vector;
open Scene;

let shadeOrBackground background shader = function
    | Some fragment -> shader fragment
    | _ -> background


let normalToColourShader intersectedObject = 
    let (Vector (x, y, z)) = intersectedObject.intersection.n
    Colour.fromRGB x y z |> Colour.map (fun c -> (c + 1.0) / 2.0)

let directionalLightShader intersectedObject =
    let normal = intersectedObject.intersection.n
    let colour = intersectedObject.sceneObject.Material.colour;
    let lightDirection = -Vector(1.0, -1.0, 3.0) |> normalise
    scaleColour (lightDirection .* normal) colour

