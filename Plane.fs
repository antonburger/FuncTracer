module Plane

open Ray
open Vector

let plane p0 n r =
// Plane equation: (p - p0).n = 0
    let eps = 0.0000001
    let num = (p0 - r.o) .* n 
    let denom = r.d .* n 
    if abs denom < eps then
        if num < eps
            then seq [ { t = 0.0; p = r.o; n = n } ]
            else Seq.empty
    else
        let t = num / denom
        seq [ { t = t; p = r.o + t * r.d; n = n } ]