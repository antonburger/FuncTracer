module Ray
open Colour
open Texture

type Material = { 
    colour: Texture<Colour>; 
    reflectance: float; 
    shineyness: float 
    }

let mattWhite = { colour=(fun _->white); reflectance=0.0; shineyness= 0.0}
type Ray = { o: Point; d: Vector }

let shiftOrigin distance ray = {ray with o=ray.o+distance*ray.d}

let jitterDirection angle ray = 
    { ray with d = Jitter.jitterVector 1 angle ray.d |> Seq.head }


type RayIntersection = { 
        t: float; 
        p: Point; 
        n: Vector;
        material: Material;
        uv: TextureCoords
    }

let newIntersection = { t=0.0; p=Point.Zero; n=Vector.unitX; material=mattWhite; uv=(0.0,0.0) }

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

let setMaterial material object = object >> Seq.map (fun v-> {v with material=material})

let hueShift angle object = 
    object >> Seq.map (fun v ->
            let shifted = v.material.colour >> (Colour.hueShift angle)
            {v with material = { v.material with colour = shifted }}
        )

let textureDiffuse texture (object:Geometry):Geometry =
    object >> Seq.map 
        (fun ri -> {ri with material = {ri.material with colour = texture ri.uv}})