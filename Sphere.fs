module Sphere

open Ray
open Vector

type Sphere(centre: Point, radius: float) =
    member this.Centre = centre
    member this.Radius = radius
    interface Intersectable with
        member this.Intersect r =
            let ov = toVector r.o
            let cv = toVector this.Centre
            let a = r.d .* r.d
            let b = 2.0 * ((ov .* r.d) - (cv .* r.d))
            let c = (cv .* cv) + (ov .* ov) - 2.0 * (ov .* cv) - this.Radius ** 2.0
            let discriminant = b ** 2.0 - 4.0 * a * c
            if discriminant < 0.0 then Seq.empty
            else
                let sq = sqrt discriminant
                let twoa = 2.0 * a
                let intersection t =
                    let p = addP r.o (mulV t r.d)
                    { t = t; p = p; n = (subP p this.Centre) |> normalise }
                seq [ (-b + sq) / twoa; (-b - sq) / twoa ] |>
                Seq.map intersection

let sphere centre radius =
    Sphere(centre, radius)
