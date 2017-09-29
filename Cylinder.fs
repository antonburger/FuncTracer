module Cylinder

open Ray
open Vector

type Cylinder() =
    interface Intersectable with
        member this.Intersect r =
            let (Point (ox, oy, oz)) = r.o
            let (Vector (dx, dy, dz)) = r.d
            let a = dx * dx + dz * dz
            let b = 2.0 * (ox * dx + oz * dz)
            let c = ox * ox + oz * oz - 1.0
            let discriminant = b * b - 4.0 * a * c
            if discriminant < 0.0 then Seq.empty
            else
                let sq = sqrt discriminant
                let twoa = 2.0 * a
                let intersection t =
                    let (Point (px, py, pz)) = r.o + t * r.d
                    let n = Vector (px, 0.0, pz) |> normalise
                    { t = t; p = Point (px, py, pz); n = if n .* r.d < 0.0 then n else -n }
                seq [ (-b + sq) / twoa; (-b - sq) / twoa ] |>
                Seq.map intersection |>
                Seq.filter (fun { p = Point(_, py, _) } -> py >= 0.0 && py <= 1.0)
