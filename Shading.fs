module Shading
open Ray;
open Image;
open Vector;
open Light
open Scene;

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
    | Directional v -> if (intersectsAny scene { o = point; d = -v }) then 0.0 else 1.0
    | SoftDirectional (direction, samples, scattering) -> softShadowLightIntensity direction samples scattering scene point

let lightDirection light = 
    match light with
        | Directional v -> v
        | SoftDirectional (v, _, _) -> v

let singleLightShader lightIntensity light intersectedObject = 
    match light with 
    | ( v, lightColour) -> 
        let normal = intersectedObject.intersection.n 
        let colour = intersectedObject.sceneObject.Material.colour * lightColour 
        scaleColour (lightIntensity*(-(lightDirection v) .* normal)) colour 

let diffuseShader fragment =
    let intersectedObject = fragment.intersectedObject
    let normal = intersectedObject.intersection.n 
    let (lightColour, lightDirection)= fragment.light
    let intensity= (-lightDirection) .* normal
    scaleColour intensity (intersectedObject.sceneObject.Material.colour * lightColour)

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
        let intensity = shadowLightIntensity scene lightConfig shadowRayOrigin
        (scaleColour intensity colour, (lightDirection lightConfig) );
    )

let createFragments getColourForDirection scene ray someIntersection = 
    match someIntersection with 
    | None -> Seq.empty
    | Some intersection -> 
        getLightsOnPoint scene intersection 
        |> Seq.map (fun light -> {
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
    createFragments getColourForDirection scene ray |>
    Seq.sumBy shader 

let shade (shader:Shader) scene (pixelRays : seq<(int * int) * Ray list>) = 
    let push a b = (a,b)
    let shadePixel =  Seq.averageBy (getColourForRay shader scene 3)
    Seq.map (fun pixelRay -> async { return (fst pixelRay, shadePixel <| snd pixelRay) }) pixelRays |> Async.Parallel |> Async.RunSynchronously 

