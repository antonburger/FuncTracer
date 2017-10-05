module Cube

open Ray
open Vector
open Plane
open Point
open Transform


let square = plane (Point (0.0,0.0,0.0)) (Vector (0.0,1.0,0.0)) >>
        Seq.filter (
            fun v -> 
                let (Point(x,y,z)) = v.p
                (x>=0.0) && (x<=1.0) && (z>=0.0) && (z<=1.0)
        )

let cube = 
    let bottom = flipNormals square
    let top = transform (translate (Vector(0.0,1.0,0.0))) square 
    let left = transform (rotate unitZ (Deg.toRad 90.0<deg>)) square
    let right = flipNormals <| transform (translate unitX) left
    let front = transform (rotate unitX (Deg.toRad -90.0<deg>)) square
    let back = flipNormals <| transform (translate unitZ) front
    let combined = group [bottom; top; left; right;front;back] 
    (combined |> transform (translate (Vector(-0.5, -0.5, -0.5))))

