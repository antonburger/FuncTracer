module Sphere

open Ray
open Vector

type private Sphere() =
    interface Intersectable with
        member this.Intersect r =
            let ov = Point.toVector r.o
            let a = r.d .* r.d
            let b = 2.0 * (ov .* r.d)
            let c = (ov .* ov) - 1.0
            let intersection t =
                let p = r.o + t * r.d
                { t = t; p = p; n = Point.toVector p |> normalise }
            Math.quadratic a b c |>
            Seq.map intersection

let sphere = Sphere() :> Intersectable
