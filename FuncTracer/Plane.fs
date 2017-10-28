module Plane

open Ray
open Vector
open Texture

type Plane = Plane of Point*Vector

let intersect (Plane(p0,n)) r=
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

let isAbove (Plane(p0,n)) (point:Point) = 
    (point-p0).*n >=0.0
let split plane points =
    points |> Seq.where (isAbove plane ),
    points |> Seq.where (isAbove plane >> not)

let private setUV r = 
    let (Point (x,y,z)) = r.p
    {r with uv = (x,z) }

let plane =
    intersect (Plane(Point.Zero, unitY)) >> Seq.map setUV