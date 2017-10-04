module Ray

type Ray = { o: Point; d: Vector }

type RayIntersection = { t: float; p: Point; n: Vector }

type IntersectableFunc = (Ray->RayIntersection seq)

let combine (intersectables:IntersectableFunc seq) = (fun r->intersectables |> Seq.collect (fun v->v r))

let flipNormals i = i >> Seq.map (fun v->{v with n = (-1.0*v.n) }) 
