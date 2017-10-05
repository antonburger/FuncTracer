module Shading
open Ray;
open Image;
open Vector;
open Light
open Scene;
open FSharp.Collections.ParallelSeq

type Fragment  = {
    intersectedObject: IntersectedObject; 
    light: Colour*Vector;
    viewRay: Ray
    getColour: Vector->Colour
}

type Shader = (Fragment -> Colour) 

let shadeOrBackground background shader = function 
    | Some fragment -> shader fragment 
    | _ -> background 

let softShadowLightIntensity direction samples scattering (intersectsScene: Ray -> bool) origin =
    let intersectsInDirection d = intersectsScene { o = origin; d = d}
    let occludedSampleCount = 
        Jitter.jitterVector samples scattering -direction
        |> Seq.map intersectsInDirection
        |> Seq.where id 
        |> Seq.length
    (float)(samples-occludedSampleCount)/(float)samples

let shadowLightIntensity (intersectsScene: Ray -> bool) light point =
    match light with
    // Hard shadow determination - in or out.
    | Directional v -> if (intersectsScene { o = point; d = -v }) then 0.0 else 1.0
    | SoftDirectional (direction, samples, scattering) -> softShadowLightIntensity direction samples scattering intersectsScene point

let lightDirection light = 
    match light with
        | Directional v -> v
        | SoftDirectional (v, _, _) -> v

let diffuseShader fragment =
    let intersectedObject = fragment.intersectedObject
    let normal = intersectedObject.intersection.n 
    let (lightColour, lightDirection)= fragment.light
    let intensity= (-lightDirection) .* normal
    scaleColour intensity (intersectedObject.sceneObject.Material.colour * lightColour)

let specularShader fragment = 
    let intersectedObject = fragment.intersectedObject
    let normal = intersectedObject.intersection.n |> normalise
    let (lightColour, lightDirection)= fragment.light
    let shineyness = intersectedObject.sceneObject.Material.shineyness
    let reflectedLightDirection = Vector.reflect normal lightDirection |> Vector.normalise 
    let viewDirection = fragment.viewRay.d |> Vector.normalise
    let intensity = (viewDirection.*(-reflectedLightDirection)) ** shineyness
    if (shineyness<=0.0 || intensity<=0.0) then Colour.black else
        lightColour |> Colour.map (fun v->v*intensity)

let reflectionShader fragment = 
    let intersectedObject = fragment.intersectedObject
    let material = intersectedObject.sceneObject.Material
    let normal = intersectedObject.intersection.n 
    let viewDirection = fragment.viewRay.d
    let reflectedDirection = Vector.reflect normal viewDirection
    if (material.reflectance>0.0) then
        fragment.getColour reflectedDirection |> Colour.map(fun v->v*material.reflectance)
    else
        Colour.Zero

let multiPartShader (shaders:Shader list) fragment = 
    shaders 
    |> Seq.sumBy (fun v->v fragment)

let getLightsOnPoint scene intersectedObject =
    // Project shadow rays from fractionally above the intersected point in order to avoid speckling from self-intersections.
    let shadowRayOrigin = intersectedObject.intersection.p + 0.0001 * intersectedObject.intersection.n
    scene.lights |> 
    Seq.map (fun light -> 
        let (Light (lightConfig, colour)) = light
        let intensity = shadowLightIntensity (intersectsAny scene) lightConfig shadowRayOrigin
        (scaleColour intensity colour, (lightDirection lightConfig) );
    )

let createFragments getColourForDirection scene ray intersection =
    getLightsOnPoint scene intersection
    |> Seq.map (fun light ->
            {
            intersectedObject= intersection;
            light=light;
            viewRay = ray;
            getColour=getColourForDirection intersection.intersection.p
            })

let slightOffset (ray:Ray) = {o=ray.o + 0.0001 * ray.d; d=ray.d}

let rec getColourForRay shader scene recursionLimit ray =
    let getColourForDirection point direction= 
        if (recursionLimit<=0) then Colour.black else
        getColourForRay shader scene (recursionLimit-1) {o=point; d=direction}
    slightOffset ray |>
    intersectScene scene |>
    Option.map (createFragments getColourForDirection scene ray) |>
    Option.defaultValue Seq.empty |>
    Seq.sumBy shader

let shade (shader:Shader) scene (pixelRays : seq<PixelCoord * Ray list>) =
    let shadePixel = Seq.averageBy (getColourForRay shader scene 8)
    pixelRays
    |> PSeq.ordered
    |> PSeq.map (fun pixelRay -> (fst pixelRay, shadePixel <| snd pixelRay))
    :> seq<PixelCoord * Colour>
