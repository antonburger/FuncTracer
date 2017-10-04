module Cone

open Ray
open Vector

type private Cone() =
    interface Intersectable with
        member this.Intersect r =
            let { o = Point (ox, oy, oz); d = Vector (dx, dy, dz) } = r
            // Perform some reference frame shenanigans here to make sure the
            // default "cone" consists only of the -y subset of the whole
            // surface, with the base at the origin and the apex at the top, 1
            // unit higher.
            // Start by shifting the ray's origin down by our canonical height,
            // before solving the intersection equation.
            let oy = oy - 1.0
            let a = dx * dx + dz * dz - dy * dy
            let b = 2.0 * (ox * dx + oz * dz - oy * dy)
            let c = ox * ox + oz * oz - oy * oy
            let intersection t =
                let (Point (px, py, pz)) = (Point (ox, oy, oz)) + t * r.d
                // Shift the intersected point back into the desired frame.
                let p = Point (px, py + 1.0, pz)
                // But leave the normal as is - not affected by translation.
                let n = Vector (px, -py, pz) |> normalise
                { t = t; p = p; n = if n .* r.d < 0.0 then n else -n }
            Math.quadratic a b c |>
            Seq.map intersection |>
            Seq.filter (fun { p = Point(_, py, _) } -> py >= 0.0 && py <= 1.0)

let cone = Cone() |> toIntersectableFunc