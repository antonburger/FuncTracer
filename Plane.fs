module Plane

open Ray
open Vector

// Plane equation: (p - p0).n = 0
type Plane(p0 : Point, n : Vector) =
    member this.P0 = p0
    member this.N = normalise n
    interface Intersectable with
        member this.Intersect r =
            let eps = 0.0000001
            let num = dot (subP this.P0 r.o) this.N
            let denom = dot r.d this.N
            if abs denom < eps then
                if num < eps
                    then seq [ { t = 0.0; p = r.o; n = this.N } ]
                    else Seq.empty
            elif sign num = sign denom then
                let t = num / denom
                seq [ { t = t; p = addP r.o (mulV t r.d); n = this.N } ]
            else
                Seq.empty
