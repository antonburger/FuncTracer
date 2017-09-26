module Scene
open Ray
open Image

type Material = {colour:Colour}
type SceneObject(geometry: Intersectable, material: Material) =
    member this.Geometry = geometry
    member this.Material = material
    interface Intersectable with
        member this.Intersect r = this.Geometry.Intersect r

type Scene = Scene of SceneObject list
type IntersectedObject = { intersection: RayIntersection; sceneObject: SceneObject }

let closest intersections =
    let rayDistance = fun { intersection={ t = t } } -> t
    let candidates = intersections |>
                     Seq.sortBy rayDistance |>
                     Seq.skipWhile (rayDistance >> (>) 0.0)
    if Seq.isEmpty candidates
        then None
        else Some <| Seq.head candidates

let intersectScene (Scene objects) r =
    let flip f = fun x y -> f y x
    objects |> Seq.collect ( 
                fun obj -> flip intersect r obj 
                        |> Seq.map(fun v->{intersection=v; sceneObject=obj})
        ) |> closest
