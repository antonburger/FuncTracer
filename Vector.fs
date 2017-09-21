module Vector

open Ray

let dot (Vector (x1,y1,z1)) (Vector (x2,y2,z2)) =
    x1 * x2 + y1 * y2 + z1 * z2

let cross (Vector (x1,y1,z1)) (Vector (x2,y2,z2)) =
    Vector (y1 * z2 - z1 * y2, x2 * z1 - z2 * x1, x1 * y2 - y1 * x2)

let (.*) x y = dot x y

let mulV (f: float) (Vector (x,y,z)) =
    Vector (f * x, f * y, f * z)

let negV =
    mulV -1.0

let addV (Vector (x1,y1,z1)) (Vector (x2,y2,z2)) =
    Vector (x1 + x2, y1 + y2, z1 + z2)

let subV v1 v2 =
    addV v1 (negV v2)

let toVector (Point (x,y,z)) =
    Vector (x,y,z)

let toPoint (Vector (x,y,z)) =
    Point (x,y,z)

let addP p v =
    addV (toVector p) v |> toPoint

let subP p1 p2 =
    subV (toVector p1) (toVector p2)

let len vec = vec .* vec |> sqrt

let normalise vec =
    let l = len vec
    if l < 0.0000001
        then vec
        else mulV (1.0/l) vec
