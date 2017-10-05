module Ray

type Ray = { o: Point; d: Vector }

type RayIntersection = { t: float; p: Point; n: Vector }

type Geometry = (Ray->RayIntersection seq)
type Solid = (Ray->RayIntersection seq)

let group x = (fun r-> x |> Seq.collect ((|>)r) )

let flipNormals i = i >> Seq.map (fun v->{v with n = (-1.0*v.n) }) 
