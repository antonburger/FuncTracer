module Ray

type Ray = { o: Point; d: Vector }

type RayIntersection = { t: float; p: Point; n: Vector }

type IntersectableFunc = (Ray->RayIntersection seq)

type Intersectable =
    abstract member Intersect: Ray -> RayIntersection seq


let toIntersectableFunc (i:Intersectable) = i.Intersect
let intersect (i:IntersectableFunc) r = i r

type private CombinedIntersectable(intersectables) = 
    interface Intersectable with
        member this.Intersect r = intersectables |> Seq.collect (fun i -> intersect i r)

type private ComposedIntersectable(i:Intersectable,f)=
    interface Intersectable with
        member this.Intersect r = i.Intersect r |>f


let combine x = CombinedIntersectable(x) |> toIntersectableFunc
let flipNormals i = i >> Seq.map (fun v->{v with n = (-1.0*v.n) }) 