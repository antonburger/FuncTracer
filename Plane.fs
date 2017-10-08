module Plane

open Ray
open Vector
open Texture

let private setUV r = 
    let (Point (x,y,z)) = r.p
    {r with uv = (x-floor x, z-floor z) }

let plane =
    let intersect r = 
        let p0 = Point.Zero
        let n = Vector.unitY
    // Plane equation: (p - p0).n = 0
        let eps = 0.0000001
        let num = (p0 - r.o) .* n 
        let denom = r.d .* n 
        if abs denom < eps then
            if num < eps
                then seq [ { newIntersection with t = 0.0; p = r.o; n = n } ]
                else Seq.empty
        else
            let t = num / denom
            seq [ {newIntersection with  t = t; p = r.o + t * r.d; n = n } ]
    intersect >> Seq.map setUV