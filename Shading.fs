module Shading
open Ray;
open Image;
open Vector;
open Light
open Scene;

type Fragment  = {
    intersectedObject: IntersectedObject; 
    scene: Scene
}

type Shader = (Fragment -> Colour)

let shadeOrBackground background shader = function 
    | Some fragment -> shader fragment 
    | _ -> background 

// Hard shadow determination - in or out. Probably want to change this to a shadow factor when there are area lights.
let isInShadow scene light point =
    match light with
    | DirectionalLight v -> intersectsAny scene { o = point; d = -v }

let singleLightShader light intersectedObject = 
    match light with 
    | (DirectionalLight v, lightColour) -> 
        let normal = intersectedObject.intersection.n 
        let colour = intersectedObject.sceneObject.Material.colour * lightColour 
        scaleColour (-v .* normal) colour 

let lightShader fragment =
    let intersectedObject = fragment.intersectedObject
    let scene = fragment.scene
    // Arbitrary offset to combat shadow artifacts. 
    let i = fragment.intersectedObject.intersection;
    let shadowRayOrigin = i.p + i.n * 0.0001 
    scene.lights |> 
    Seq.map (fun l -> if isInShadow scene (fst l) shadowRayOrigin then Colour.black else singleLightShader l intersectedObject) |> 
    // Accumulate and clamp the colours per light. 
    Seq.fold (+) Colour.black |> 
    Colour.map clamp 


let createFragment scene someIntersection = 
    match someIntersection with 
    | None -> None
    | Some intersection -> Some { scene=scene; intersectedObject=intersection }

let shade shader scene (pixelRays : seq<(int * int) * Ray list>) = 
    let shadePixel (rays : Ray list) = 
        rays |> 
        Seq.map (intersectScene scene >> (createFragment scene) >> shader) |> 
        Seq.fold (+) Colour.black |> 
        Colour.map (fun c -> c / float rays.Length) 
    Seq.map (fun pixelRay -> async { return (fst pixelRay, shadePixel <| snd pixelRay) }) pixelRays |> Async.Parallel |> Async.RunSynchronously 

