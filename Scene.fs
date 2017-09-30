module Scene

open Light
open Ray
open Image

module Seq = 
    let firstOrNone seq = 
        if Seq.isEmpty seq
            then None
            else Some <| Seq.head seq

type SceneOptions = {
    camera : Camera;
    multisampleCount : int;
    resolution : Resolution;
}
with static member Default = {
        camera = { o = Point (0.0, 0.0, 0.0); lookAt = Point(0.0, 0.0, 1.0); up = Vector (0.0, 1.0, 0.0); fovY = Deg.toRad 50.0<deg>; aspectRatio = 1.0};
        multisampleCount = 8;
        resolution = Resolution (400, 400);
    }


type Material = { colour:Colour; reflectance: float; shineyness: float }
type SceneObject(geometry: Intersectable, material: Material) =
    member this.Geometry = geometry
    member this.Material = material
    interface Intersectable with
        member this.Intersect r = this.Geometry.Intersect r

type Scene = {
    objects: SceneObject list;
    lights: Light list;
}

module Scene = 
    let addObject obj scene = {scene with objects=obj::scene.objects}

type IntersectedObject = { intersection: RayIntersection; sceneObject: SceneObject }



let sortByDistance = 
    let rayDistance = fun { intersection={ t = t } } -> t
    Seq.sortBy rayDistance >>
    Seq.skipWhile (rayDistance >> (>) 0.0)

let closest = sortByDistance >> Seq.firstOrNone

let getAllIntersections ({ objects = objects }) r =
    let flip f = fun x y -> f y x
    objects |> Seq.collect ( 
                fun obj -> flip intersect r obj 
                        |> Seq.map(fun v->{intersection=v; sceneObject=obj})
        ) 

let intersectScene scene  = getAllIntersections scene >>  closest

// Just look for *an* intersection. Not interested in the closest one.
let intersectsAny ({ objects = objects }) r =
    let flip f = fun x y -> f y x
    objects |>
    Seq.collect (flip intersect r) |>
    Seq.exists (fun i -> i.t >= 0.0)
