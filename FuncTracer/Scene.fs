module Scene

open Light
open Ray
open Image
open Transform

type Primitive =    
    | BspMesh of Geometry
    | Circle
    | Square
    | Cube
    | Sphere
    | Plane
    | Cone
    | SolidCylinder
    | Cylinder
    | Triangle of Triangle.Triangle

let intersectPrimitive = function
    | BspMesh g -> g
    | Circle -> Cylinder.circle
    | Square -> Cube.square
    | Cube   -> Cube.cube
    | Sphere -> Sphere.sphere
    | Plane  -> Plane.plane
    | Cone   -> Cone.cone
    | SolidCylinder ->  Cylinder.solidCylinder 
    | Cylinder -> Cylinder.cylinder
    | Triangle t -> Triangle.triangle t


type SceneGraph = 
    | Primitive     of primitive: Primitive
    | SceneFunction of SceneFunction*SceneGraph
    | Group         of nodes: SceneGraph list
    | Union         of SceneGraph*SceneGraph
    | Intersect     of SceneGraph*SceneGraph
    | Subtract      of SceneGraph*SceneGraph
    | Exclude       of SceneGraph*SceneGraph
and SceneFunction =
    | Transform     of Transform.Transform
    | Material      of material : Material
    | Texture       of source:  Texture
    | HueShift      of angle: float
    | IgnoreLight
and Texture = 
    | Image of Texture.Texture
    | Grid of Colour*Colour
    | TextureFunction of Texture*TextureFunction
and TextureFunction = 
    | Scale of float*float
    | Rotate of float<rad>


type SceneOptions = {
    camera : Camera;
    samplingStrategy: ImagePlane -> SamplingStrategy;
    resolution : Resolution;
}
with static member Default = {
        camera = { o = Point (0.0, 0.0, 0.0); lookAt = Point(0.0, 0.0, 1.0); up = Vector (0.0, 1.0, 0.0); fovY = Deg.toRad 50.0<deg>; aspectRatio = 1.0; focus=None};
        samplingStrategy = JitteredSampling.strategy 8;
        resolution = Resolution (400, 400);
    }

let rec intersect scene = 
    let getTextureFunction texture = function
        | Scale (a,b)     -> Texture.scale (a,b) texture
        | Rotate angle    -> Texture.rotate angle texture
    let rec getTexture = function 
        | Image i         -> i
        | Grid (a,b)      -> Texture.grid a b
        | TextureFunction (texture,f) -> getTextureFunction (getTexture texture) f
    let sceneFunction:(SceneFunction->Geometry->Geometry) = function
        | Transform     t -> transform t 
        | Material      m -> setMaterial m 
        | Texture       t -> textureDiffuse (getTexture t) 
        | HueShift      a -> hueShift a 
        | IgnoreLight     -> ignoreLight 
    match scene with 
        | Primitive     p     -> intersectPrimitive p
        | SceneFunction (a,b) -> 
            let b' = intersect b // 'let' required so that the entire tree is evaluated to a function before execution
            b' |> sceneFunction a 
        | Union         (a,b) -> 
            let a' = intersect a
            let b' = intersect b
            Csg.union a' b'
        | Subtract         (a,b) -> 
            let a' = intersect a
            let b' = intersect b
            Csg.subtract a' b'
        | Intersect         (a,b) -> 
            let a' = intersect a
            let b' = intersect b
            Csg.intersect a' b'
        | Exclude         (a,b) -> 
            let a' = intersect a
            let b' = intersect b
            Csg.exclude a' b'
        | Group         nodes -> 
        let nodes' = Seq.map intersect (nodes) |> Seq.toList 
        nodes' |> group


type Scene = {
    objects: SceneGraph;
    lights: Light list;
}

let sortByDistance:(RayIntersection seq -> RayIntersection seq) = 
    let rayDistance = (fun r->r.t)
    Seq.sortBy rayDistance >>
    Seq.skipWhile (rayDistance >> (>) 0.0)
let closest:(RayIntersection seq -> RayIntersection option) = sortByDistance >> Seq.tryHead
let sceneGeometry ({ objects = objects }) = intersect objects 
let intersectScene geometry  = geometry >> closest
let lightIsBocked geometry maxDistance =
    let flip f = fun x y -> f y x
    geometry  >> Seq.exists (fun i -> i.t >= 0.0 && i.t < maxDistance && i.material.applyLighting)