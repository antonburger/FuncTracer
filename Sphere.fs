module Sphere

open Ray
open Vector

type Sphere() =
    interface Intersectable with
        member this.Intersect r =
            let ov = Point.toVector r.o
            let cv = Point.toVector (Point (0.0,0.0,0.0)) (* this.Centre *)
            let a = r.d .* r.d
            let b = 2.0 * ((ov .* r.d) - (cv .* r.d))
            let c = (cv .* cv) + (ov .* ov) - 2.0 * (ov .* cv) - 1.0 (* this.Radius ** 2 *)
            let discriminant = b ** 2.0 - 4.0 * a * c
            if discriminant < 0.0 then Seq.empty
            else
                let sq = sqrt discriminant
                let twoa = 2.0 * a
                let intersection t =
                    let p = r.o + t * r.d
                    { t = t; p = p; n = p - (Point (0.0,0.0,0.0)) (* this.Centre *) |> normalise }
                seq [ (-b + sq) / twoa; (-b - sq) / twoa ] |>
                Seq.map intersection
