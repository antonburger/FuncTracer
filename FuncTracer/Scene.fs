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
    | SceneFunction of SceneGraph*SceneFunction
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

let rec intersect (graph:SceneGraph) : Geometry = 
    let getTextureFunction texture = function
        | Scale (a,b)     -> Texture.scale (a,b) texture
        | Rotate angle    -> Texture.rotate angle texture
    let rec getTexture = function 
        | Image i         -> i
        | Grid (a,b)      -> Texture.grid a b
        | TextureFunction (texture,f) -> getTextureFunction (getTexture texture) f
    let sceneFunction argument = function
        | Transform     t -> transform t (intersect argument)
        | Material      m -> Ray.setMaterial m (intersect argument)
        | Texture       t -> textureDiffuse (getTexture t) (intersect argument)
        | HueShift      a -> hueShift a (intersect argument)
        | IgnoreLight     -> ignoreLight (intersect argument)
    match graph with
    | Primitive     p     -> intersectPrimitive p
    | SceneFunction (a,b) -> sceneFunction a b
    | Union         (a,b) -> Csg.union (intersect a) (intersect b)
    | Subtract      (a,b) -> Csg.subtract (intersect a) (intersect b)
    | Intersect     (a,b) -> Csg.intersect (intersect a) (intersect b)
    | Exclude       (a,b) -> Csg.exclude (intersect a) (intersect b)
    | Group         nodes -> Seq.map intersect (nodes) |> group


type Scene = {
    objects: SceneGraph;
    lights: Light list;
}

let sortByDistance = 
    let rayDistance = (fun r->r.t)
    Seq.sortBy rayDistance >>
    Seq.skipWhile (rayDistance >> (>) 0.0)

let closest = sortByDistance >> Seq.tryHead

let getAllIntersections ({ objects = objects }) = intersect objects 

let intersectScene scene  = getAllIntersections scene >>  closest

let lightIsBocked scene maxDistance =
    let flip f = fun x y -> f y x
    (getAllIntersections scene)  >> Seq.exists (fun i -> i.t >= 0.0 && i.t < maxDistance && i.material.applyLighting)