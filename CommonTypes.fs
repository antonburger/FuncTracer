[<AutoOpen>]
module CommonTypes

type Vector = Vector of (float * float * float) with
    static member (+) (Vector (x1, y1, z1), Vector (x2, y2, z2)) =
        Vector (x1 + x2, y1 + y2, z1 + z2)
    static member (*) (Vector (x, y, z), s) =
        Vector (s * x, s * y, s * z)
    static member (*) (s : float, v : Vector) =
        v * s
    static member (~-) (Vector (x, y, z)) =
        Vector (-x, -y, -z)
    static member (-) (v1 : Vector, v2 : Vector) =
        v1 + -v2
    static member (.*) (Vector (x1, y1, z1), Vector (x2, y2, z2)) =
        x1 * x2 + y1 * y2 + z1 * z2
    static member (.**) (Vector (x1, y1, z1), Vector (x2, y2, z2)) =
        Vector (y1 * z2 - z1 * y2, x2 * z1 - z2 * x1, x1 * y2 - y1 * x2)
    member this.Length = this .* this |> sqrt

type Point = Point of (float * float * float) with
    static member (+) (Point (px, py, pz), Vector (vx, vy, vz)) =
        Point (px + vx, py + vy, pz + vz)
    static member (+) (v : Vector, p : Point) =
        p + v
    static member (-) (Point (x1, y1, z1), Point (x2, y2, z2)) =
        Vector (x1 - x2, y1 - y2, z1 - z2)

let toVector (Point (x, y, z)) =
    Vector (x, y, z)

let toPoint (Vector (x, y, z)) =
    Point (x, y, z)
