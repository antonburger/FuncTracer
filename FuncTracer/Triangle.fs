module Triangle
open Ray
open CommonTypes
open CommonTypes.Vector

let private ``Möller–Trumbore intersection algorithm`` triangle ray= 
    let epsilon= 0.0000001 
    let (vertex0, vertex1, vertex2) = triangle
    let edge1 = vertex1 - vertex0
    let edge2 = vertex2 - vertex0
    let h = ray.d.**edge2
    let a = edge1.*h
    if (a > -epsilon && a < epsilon) then Seq.empty
    else
        let f = 1.0/a
        let s = ray.o - vertex0
        let u = f * (s.*h)
        if (u < 0.0 || u > 1.0) then Seq.empty
        else
            let q = s.**edge1
            let v = f * ray.d.*q
            if (v < 0.0 || u + v > 1.0) then Seq.empty
            else
            // At this stage we can compute t to find out where the intersection point is on the line.
                let t = f * edge2.*q
                if (t > epsilon) then 
                    let point = ray.o + ((ray.d |> normalise) * (t * ray.d.Length))
                    Seq.singleton {newIntersection with t=t; p = point; n = normalise (edge1.**edge2) }
                else 
                    Seq.empty


let triangle a b c = ``Möller–Trumbore intersection algorithm`` (a,b,c) 