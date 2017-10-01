module Sphere

open Ray
open Vector

let sphere ray =
    let ov = Point.toVector ray.o
    let a = ray.d .* ray.d
    let b = 2.0 * (ov .* ray.d)
    let c = (ov .* ov) - 1.0
    let intersection t =
        let p = ray.o + t * ray.d
        { t = t; p = p; n = Point.toVector p |> normalise }
    Math.quadratic a b c |>
    Seq.map intersection
