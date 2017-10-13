module Sphere

open Ray
open Vector

let private setUV r = 
    let (Vector(dx, dy, dz)) = -r.n
    let u = 0.5 + ((atan2 dz dx)/(2.0*System.Math.PI))
    let v = 0.5 - (asin dy)/System.Math.PI
    {r with uv = (u,v)}
let sphere r =
    let ov = Point.toVector r.o
    let a = r.d .* r.d
    let b = 2.0 * (ov .* r.d)
    let c = (ov .* ov) - 1.0
    let intersection t =
        let p = r.o + t * r.d
        { newIntersection with  t = t; p = p; n = Point.toVector p |> normalise } 
        |> setUV
    Math.quadratic a b c |>
    Seq.map intersection 
