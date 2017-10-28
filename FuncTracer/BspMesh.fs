module BspMesh
open Ray
open BoundingBox
open CommonTypes
open Triangle
open Plane

let test condition ifTrue ifFalse x = if (condition x) then ifTrue x else ifFalse x



type BspTree = {
    aabb: BoundingBox;
    left: BspNode;
    right: BspNode
}
and BspNode = 
    Branch of BspTree 
    | Leaf of Geometry

type BoundedGeometry  = {
    aabb: BoundingBox;
    geometry: Geometry
} with member this.toGeometry = test (BoundingBox.intersects this.aabb) this.geometry (fun r->Seq.empty)
 
let boundedGeometry aabb geometry = {aabb=aabb; geometry=geometry}

let trianglePoints (Triangle(a,b,c)) = [a;b;c] |> List.toSeq

let optimalSplit aabb triangles = 
    let widthx = abs (aabb.max.x - aabb.min.x)/2.0
    let widthy = abs (aabb.max.y - aabb.min.y)/2.0
    let widthz = abs (aabb.max.z - aabb.min.z)/2.0
    let partiaionPlane = 
        if (widthx>widthy && widthx>widthz) then 
            Plane(Point((aabb.min.x+aabb.max.x)/2.0,0.0,0.0),Vector.unitX)
        else if (widthy>widthz) then 
                Plane(Point(0.0,(aabb.min.y+aabb.max.y)/2.0,0.0),Vector.unitY)
            else 
                Plane(Point(0.0,0.0,(aabb.min.z+aabb.max.z)/2.0),Vector.unitZ)
    let splitTris = triangles |> Seq.map (slice partiaionPlane)    
    let res = (
        splitTris |> Seq.collect fst,
        splitTris |> Seq.collect snd
        )
    res


let trianglesBoundry (tris:Triangle seq) =  Seq.collect trianglePoints tris |> pointsBoundry

let rec compile maxDepth (triangles:Triangle seq):BspNode= 
    let leaf = 
        Leaf(triangles |> Seq.map triangle |> group)
    if (maxDepth=0) then leaf
    else
        let aabb = trianglesBoundry triangles 
        let left,right = optimalSplit aabb triangles
        let triCount = Seq.length triangles
        if (Seq.length left >= triCount || Seq.length right >= triCount) 
        then leaf
        else
            Branch({
            aabb=aabb;
            left=compile (maxDepth-1) left;
            right=compile (maxDepth-1) right
            })

let rec intersect showBoxes (tree:BspTree) ray: RayIntersection seq = 
    let inline intersectNode node:Geometry = 
        match node with 
        | Branch t -> intersect showBoxes t 
        | Leaf g  -> if (showBoxes) then BoundingBox.asCube tree.aabb else g
    if (BoundingBox.intersects tree.aabb ray) then
        let right = intersectNode (tree.right) ray
        let left = intersectNode tree.left ray 
        left|>Seq.append right
    else Seq.empty
    
let rec maxDepth tree = 
    match tree with
        | Branch t -> 1+max (maxDepth t.left) (maxDepth t.right)
        | Leaf t -> 0

let rec Leaves tree = 
    match tree with
        | Branch t -> Seq.append (Leaves t.left) (Leaves t.right)
        | Leaf t -> Seq.singleton t 

let bspMesh showBoxes depth triangles = 
    printfn "Compiling bsp tree for %i triangles" (Seq.length triangles)
    let sw = new System.Diagnostics.Stopwatch()
    do sw.Start()
    let tree = compile depth triangles
    do sw.Stop()
    printfn "All done in %A, max depth %i, %i leaves "  sw.Elapsed (maxDepth tree) (Leaves tree |> Seq.length)
    match tree with
        | Branch t -> intersect showBoxes t
        | Leaf g -> g
