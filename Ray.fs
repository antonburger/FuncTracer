module Ray

type Ray = { o: Point; d: Vector }

type RayIntersection = { t: float; p: Point; n: Vector }

type Intersectable =
    abstract member Intersect: Ray -> RayIntersection seq

let intersect (i : Intersectable) r = i.Intersect(r)