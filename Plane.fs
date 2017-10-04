module Plane

open Ray
open Vector

// Plane equation: (p - p0).n = 0
type private Plane(p0 : Point, n : Vector) =
    member this.P0 = p0
    member this.N = normalise n
    interface Intersectable with
        member this.Intersect r =
            let eps = 0.0000001
            let num = (this.P0 - r.o) .* this.N
            let denom = r.d .* this.N
            if abs denom < eps then
                if num < eps
                    then seq [ { t = 0.0; p = r.o; n = this.N } ]
                    else Seq.empty
            else
                let t = num / denom
                seq [ { t = t; p = r.o + t * r.d; n = this.N } ]

let plane p0 n = Plane(p0, n) :> Intersectable
