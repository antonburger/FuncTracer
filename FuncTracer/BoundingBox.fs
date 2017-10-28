module BoundingBox
open CommonTypes
open Operators
open Transform
open Cube

type BoundingBox = { min: Point; max:Point }

let pointsBoundry (points:Point seq):BoundingBox = 
    let x = points |> Seq.map (fun v->v.x)
    let y = points |> Seq.map (fun v->v.y)
    let z = points |> Seq.map (fun v->v.z)
    let maxX = x |> Seq.max
    let minX = x |> Seq.min
    let maxY = y |> Seq.max
    let minY = y |> Seq.min
    let maxZ = z |> Seq.max
    let minZ = z |> Seq.min
    { 
        min= Point(minX, minY, minZ);
        max= Point(maxX, maxY, maxZ);
    }

let asCube aabb = 
    let (Vector(scaleVector)) = (aabb.max- aabb.min)
    let translateVector = 0.5*((Point.toVector aabb.min)+(Point.toVector aabb.max))
    cube 
    |> transform (scale scaleVector)
    |> transform (translate translateVector)

//http://www.cs.utah.edu/~awilliam/box/box.pdf
let intersects  (box:BoundingBox) (ray:Ray.Ray)= 
    let t0 = -infinity
    let t1 = infinity
    let bounds = [|box.min; box.max|]

    //These could be stored on the ray 
    let inv_direction = Vector(1.0/ray.d.x, 1.0/ray.d.y, 1.0/ray.d.z) 
    let signToInt x = if x<0.0 then 1 else 0
    let sign = [|signToInt inv_direction.x; signToInt inv_direction.y; signToInt inv_direction.z|]

    let mutable tmin = (bounds.[sign.[0]].x - ray.o.x) * inv_direction.x
    let mutable tmax = (bounds.[1-sign.[0]].x - ray.o.x) * inv_direction.x
    let tymin = (bounds.[sign.[1]].y - ray.o.y) * inv_direction.y
    let tymax = (bounds.[1-sign.[1]].y - ray.o.y) * inv_direction.y
    if ( (tmin > tymax) || (tymin > tmax) ) 
        then false
    else
        tmin <- max tymin tmin
        tmax <- min tymax tmax
        let tzmin = (bounds.[sign.[2]].z - ray.o.z) * inv_direction.z
        let tzmax = (bounds.[1-sign.[2]].z - ray.o.z) * inv_direction.z
        if ( (tmin > tzmax) || (tzmin > tmax) ) 
        then false
        else
            tmin <- max tzmin tmin
            tmax <- min tzmax tmax
            ( (tmin < t1) && (tmax > t0) )