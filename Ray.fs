module Ray

type Ray = { o: Point; d: Vector }

type RayIntersection = { 
    t: float; 
    p: Point; 
    n: Vector 
    }

type Geometry = (Ray->RayIntersection seq)
type Solid = (Ray->RayIntersection seq)

let group x = (fun r-> x |> Seq.collect ((|>)r) )

let flipNormals i = i >> Seq.map (fun v->{v with n = (-1.0*v.n) }) 

let rec private repeatLoop count f object objects= 
    let newObj = f object
    match count with
        | 1 -> objects
        | x when x>1 -> newObj :: repeatLoop (x-1) f newObj (newObj::objects)
        | _ -> objects

let repeat count f object = repeatLoop count f object [object] |> group