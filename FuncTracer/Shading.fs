module Shading
open Ray;
open Image;
open Vector;
open Light
open Scene;
open FSharp.Collections.ParallelSeq

type Fragment  = {
    intersection: RayIntersection; 
    light: Colour*Vector;
    viewRay: Ray
    getColour: Vector->Colour
}

type Shader = (Fragment -> Colour) 

let shadeOrBackground background shader = function 
    | Some fragment -> shader fragment 
    | _ -> background 


let softShadowLightIntensity direction samples scattering (intersectsScene: float -> Ray -> bool) origin =
    let intersectsInDirection d = intersectsScene System.Double.MaxValue { o = origin; d = d}
    let occludedSampleCount = 
        Jitter.jitterVector samples scattering -direction
        |> Seq.map intersectsInDirection
        |> Seq.where id 
        |> Seq.length
    (float)(samples-occludedSampleCount)/(float)samples

let shadowLightIntensity (intersectsScene: float -> Ray -> bool) light point =
    match light with
    // Hard shadow determination - in or out.
    | Directional v -> if (intersectsScene System.Double.MaxValue { o = point; d = -v }) then 0.0 else 1.0
    | SoftDirectional (direction, samples, scattering) -> softShadowLightIntensity direction samples scattering intersectsScene point
    | Point (position, falloff) ->
        let d = position - point
        let distance = d.Length
        if (intersectsScene distance { o = point; d = normalise d }) then 0.0 else
            attenuate falloff distance

let lightDirection light atPoint = 
    match light with
        | Directional v -> v
        | SoftDirectional (v, _, _) -> v
        | Point (p, _) -> atPoint - p |> normalise

let roughDiffuse (fragment:Fragment) =  
    // https://en.wikipedia.org/wiki/Oren%E2%80%93Nayar_reflectance_model
    let lightColour,lightDirection = fragment.light 
    let roughness = (fragment.intersection.material.roughness)**2.0 
    let rayAngle = angleBetween (fragment.intersection.n, -fragment.viewRay.d) 
    let lightAngle = angleBetween (fragment.intersection.n, -lightDirection) 
    let alpha = max rayAngle lightAngle 
    let beta = min rayAngle lightAngle 
    let A = 1.0- 0.5*roughness/(roughness+0.33) 
    let B = 0.45*roughness/(roughness+0.09) 
    let tangentLight = perpendicularComponent fragment.intersection.n -lightDirection |> normalise 
    let tangentRay = perpendicularComponent fragment.intersection.n -fragment.viewRay.d |> normalise 
    let intensity = (cos lightAngle)*(A+(B* 
        (max 0.0 (tangentLight.*tangentRay))*(sin alpha)*(tan beta) 
        )) 
    scaleColour intensity fragment.intersection.material.colour

let lambertianDiffuse fragment = 
    let intersectedObject = fragment.intersection
    let normal = intersectedObject.n 
    let (lightColour, lightDirection)= fragment.light
    let intensity= (-lightDirection) .* normal
    scaleColour intensity (intersectedObject.material.colour * lightColour)

let diffuseShader fragment =
    if (fragment.intersection.material.roughness=0.0) then
        lambertianDiffuse fragment
    else
        roughDiffuse fragment

let specularShader fragment = 
    let intersectedObject = fragment.intersection
    let normal = intersectedObject.n |> normalise
    let (lightColour, lightDirection)= fragment.light
    let shineyness = intersectedObject.material.shineyness
    let reflectedLightDirection = Vector.reflect normal lightDirection |> Vector.normalise 
    let viewDirection = fragment.viewRay.d |> Vector.normalise
    let intensity = (viewDirection.*(-reflectedLightDirection)) ** shineyness
    if (shineyness<=0.0 || intensity<=0.0) then Colour.black else
        lightColour |> Colour.map (fun v->v*intensity)

let reflectionShader fragment = 
    let intersectedObject = fragment.intersection
    let material = intersectedObject.material
    let normal = intersectedObject.n 
    let viewDirection = fragment.viewRay.d
    let reflectedDirection = Vector.reflect normal viewDirection
    if (material.reflectance>0.0) then
        fragment.getColour reflectedDirection |> Colour.map(fun v->v*material.reflectance)
    else
        Colour.Zero

let shadeIfRequired shader fragment = 
    if (fragment.intersection.material.applyLighting) then
        shader fragment
    else
        fragment.intersection.material.colour
let multiPartShader (shaders:Shader list) fragment = 
    shaders 
    |> Seq.sumBy (fun v->v fragment)

let getLightsOnPoint scene intersection =
    // Project shadow rays from fractionally above the intersected point in order to avoid speckling from self-intersections.
    let shadowRayOrigin = intersection.p + 0.0001 * intersection.n
    scene.lights |> 
    Seq.map (fun light -> 
        let (Light (lightConfig, colour)) = light
        let intensity = shadowLightIntensity (lightIsBocked scene) lightConfig shadowRayOrigin
        (scaleColour intensity colour, (lightDirection lightConfig intersection.p) );
    )

let createFragments getColourForDirection scene ray (intersection:RayIntersection) =
    getLightsOnPoint scene intersection
    |> Seq.map (fun light ->
            {
            intersection= intersection;
            light=light;
            viewRay = ray;
            getColour=getColourForDirection intersection.p
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

let shade (shader:Shader) scene (pixelRays : Ray seq) =
    let shadeRay = getColourForRay shader scene 8
    let chunks = Seq.chunkBySize 1000 pixelRays
    chunks
    |> PSeq.ordered
    |> PSeq.collect (Array.map shadeRay)
    :> Colour seq

