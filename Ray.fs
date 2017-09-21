module Ray

type Point = Point of (float * float * float)

type Vector = Vector of (float * float * float)

type Ray = { o: Point; d: Vector }

type Camera = { o: Point; lookAt: Point; up: Vector; fovY: float; aspectRatio: float }

type RayIntersection = { t: float; p: Point; n: Vector }

type Intersectable =
    abstract member Intersect: Ray -> RayIntersection seq

let intersect (i : Intersectable) r = i.Intersect(r)

let closest intersections =
    let rayDistance = fun { t = t } -> t
    let candidates = intersections |>
                     Seq.sortBy rayDistance |>
                     Seq.skipWhile (rayDistance >> (>) 0.0)
    if Seq.isEmpty candidates
        then None
        else Some <| Seq.head candidates
