module Ray

type Ray = { o: Point; d: Vector }

type RayIntersection = { t: float; p: Point; n: Vector }

type Intersectable =
    abstract member Intersect: Ray -> RayIntersection seq

let intersect (i : Intersectable) r = i.Intersect(r)

type private CombinedIntersectable(intersectables:Intersectable seq) = 
    interface Intersectable with
        member this.Intersect r = intersectables |> Seq.collect (fun i -> intersect i r)

type private ComposedIntersectable(i:Intersectable,f)=
    interface Intersectable with
        member this.Intersect r = i.Intersect r |>f


let combine x = CombinedIntersectable(x):>Intersectable

let flipNormals i = ComposedIntersectable(i, Seq.map (fun v->{v with n = (-1.0*v.n) }) ) :> Intersectable