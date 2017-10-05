module Scene

open Light
open Ray
open Image

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
type SceneObject(geometry: Geometry, material: Material) =
    member this.Geometry = geometry
    member this.Material = material

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

let closest = sortByDistance >> Seq.tryHead

let getAllIntersections ({ objects = objects }) r =
    let flip f = fun x y -> f y x
    objects |> Seq.collect ( 
                fun obj -> obj.Geometry r
                        |> Seq.map(fun v->{intersection=v; sceneObject=obj})
        ) 

let intersectScene scene  = getAllIntersections scene >>  closest

// Just look for *an* intersection. Not interested in the closest one.
let intersectsAny ({ objects = objects }) maxDistance r =
    let flip f = fun x y -> f y x
    objects |>
    Seq.collect ( (fun v->v.Geometry r)) |>
    Seq.exists (fun i -> i.t >= 0.0 && i.t < maxDistance)
