module Plane

open Ray
open Vector

// Plane equation: (p - p0).n = 0
let plane p0 n r =
    let eps = 0.0000001
    let num = (p0 - r.o) .* n
    let denom = r.d .* n
    if abs denom < eps then
        if num < eps
            then seq [ { t = 0.0; p = r.o; n = n } ]
            else Seq.empty
    elif sign num = sign denom then
        let t = num / denom
        seq [ { t = t; p = r.o + t * r.d; n = n } ]
    else
        Seq.empty
