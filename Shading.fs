module Shading
open Ray;
open Image;
open Vector;
open Light
open Scene;

let lightShader light intersectedObject =
    match light with
    | (DirectionalLight v, lightColour) ->
        let normal = intersectedObject.intersection.n
        let colour = intersectedObject.sceneObject.Material.colour * lightColour
        scaleColour (-v .* normal) colour

// Hard shadow determination - in or out. Probably want to change this to a shadow factor when there are area lights.
let isInShadow scene light point =
    match light with
    | DirectionalLight v ->
        intersectsAny scene { o = point; d = -v }

// TODO: Find a way to re-enable passing a custom shader here.
let shade scene (pixelRays : seq<(int * int) * Ray list>) =
    let shadeRay ray =
        let o = intersectScene scene ray
        match o with
        | None -> Colour.black
        | Some ({ intersection = i } as o2) ->
            // Arbitrary offset to combat shadow artifacts.
            let shadowRayOrigin = i.p + i.n * 0.0001
            scene.lights |>
            Seq.map (fun l -> if isInShadow scene (fst l) shadowRayOrigin then Colour.black else lightShader l o2) |>
            // Accumulate and clamp the colours per light.
            Seq.fold (+) Colour.black |>
            Colour.map clamp
    let shadePixel (rays : Ray list) =
        rays |>
        Seq.map shadeRay |>
        // Average the ray colours per pixel.
        Seq.fold (+) Colour.black |>
        Colour.map (fun c -> c / float rays.Length)
    Seq.map (fun pixelRay -> async { return (fst pixelRay, shadePixel <| snd pixelRay) }) pixelRays |> Async.Parallel |> Async.RunSynchronously
