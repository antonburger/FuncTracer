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

let softShadowLightIntensity direction samples scattering scene point = 
    let intersectsInDirection d = intersectsAny scene { o = point; d = d}
    let occludedSampleCount = 
        jitter samples scattering -direction
        |> Seq.map intersectsInDirection
        |> Seq.where id 
        |> Seq.length
    (float)(samples-occludedSampleCount)/(float)samples

// Hard shadow determination - in or out. Probably want to change this to a shadow factor when there are area lights.
let shadowLightIntensity scene light point =
    match light with
    | DirectionalLight v -> if (intersectsAny scene { o = point; d = -v }) then 0.0 else 1.0
    | SoftDirectionalLight (direction, samples, scattering) -> softShadowLightIntensity direction samples scattering scene point

let lightDirection light = 
    match light with
        | DirectionalLight v -> v
        | SoftDirectionalLight (v, _, _) -> v


let singleLightShader lightIntensity light intersectedObject = 
    match light with 
    | ( v, lightColour) -> 
        let normal = intersectedObject.intersection.n 
        let colour = intersectedObject.sceneObject.Material.colour * lightColour 
        scaleColour (lightIntensity*(-(lightDirection v) .* normal)) colour 

let lightShader fragment =
    let intersectedObject = fragment.intersectedObject
    let scene = fragment.scene
    // Arbitrary offset to combat shadow artifacts. 
    let i = fragment.intersectedObject.intersection;
    let shadowRayOrigin = i.p + i.n * 0.0001 
    scene.lights |> 
    Seq.map (fun l -> 
            let intensity = shadowLightIntensity scene (fst l) shadowRayOrigin 
            singleLightShader intensity l intersectedObject
        ) |> 
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

