module Triangle
open Ray
open CommonTypes
open CommonTypes.Vector

type Triangle = Triangle of Point*Point*Point

let edgeIntersection p a b = 
    let intersection = (Plane.intersect p { o=a; d= normalise (b-a) } |> Seq.first)
    intersection.Value.p


let private slice' plane (Triangle(a,b,c)) = //expects a on one side, b,c on the other
    let intersection a b = edgeIntersection plane a b
    let sliceSingle  = 
        Seq.singleton (Triangle(a, intersection a b, intersection a c))
    let sliceTwo  = 
        [
            (Triangle(intersection b a, b,c))
            (Triangle(c,intersection c a, intersection b a))
        ] |> List.toSeq
    (sliceSingle, sliceTwo)

let slice plane (Triangle(a,b,c)) =
    let inline flip boolean x = 
        if (boolean) then (snd x,fst x) else x
    let aAbove = Plane.isAbove plane a
    let bAbove = Plane.isAbove plane b
    let cAbove = Plane.isAbove plane c
    (
    if (aAbove = bAbove && bAbove=cAbove) then 
        (Seq.singleton (Triangle(a,b,c)),Seq.empty)
    else
        if (aAbove=bAbove) then 
            slice' plane (Triangle(c,a,b)) |> flip true
        else 
            if (aAbove=cAbove) then 
                slice' plane (Triangle(b,c,a)) |> flip true
            else 
                slice' plane (Triangle(a,b,c))
    )|>flip (not aAbove)

let private ``Möller–Trumbore intersection algorithm`` (Triangle(vertex0,vertex1,vertex2)) ray= 
    let epsilon= 0.0000001 
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


let triangle = ``Möller–Trumbore intersection algorithm`` 