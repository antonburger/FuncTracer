module Transform

open Ray
open Vector

// Private constructor because we want matrices to be determined by transforms.
type Matrix =
    private
    | Matrix of float[,]
    with
    static member (*) (Matrix m1, Matrix m2) =
        let dot j i =
            [0..3] |> List.sumBy (fun c -> (Array2D.get m1 j c) * (Array2D.get m2 c i))
        Matrix <| Array2D.init 4 4 dot
    static member (*) (Matrix m, Vector (x, y, z)) =
        let dot j =
            (Array2D.get m j 0) * x + (Array2D.get m j 1) * y + (Array2D.get m j 2) * z
        Vector (dot 0, dot 1, dot 2)
    static member (*) (Matrix m, Point (x, y, z)) =
        let dot j =
            (Array2D.get m j 0) * x + (Array2D.get m j 1) * y + (Array2D.get m j 2) * z + (Array2D.get m j 3)
        Point (dot 0, dot 1, dot 2)

// Private so we can control the values which get passed to the scale and compose constructors especially.
type Transform =
    private
    | Translate of Vector
    | Scale of (float * float * float)
    | Rotate of axis: Vector * angle: float<rad>
    | Composed of Transform list

let translate = Translate

// TODO: Bother checking for zero scales? Not invertible, but hey, if you want to divide by zero, that's your call :P
let scale = Scale

let rotate axis angle =
    Rotate (normalise axis, angle)

// Make sure a Composed contains only basic transforms, and never other Composeds.
let compose transforms =
    let listify = function
        | Composed ts -> ts
        | t -> [t]
    Composed (List.collect listify transforms)

let rec inverse = function
    | Translate v -> Translate -v
    | Scale (x, y, z) -> Scale <| (1.0 / x, 1.0 / y, 1.0 / z)
    | Rotate (axis, angle) -> Rotate (axis, -angle)
    | Composed ts -> Composed <| List.map inverse (List.rev ts)

let identity = Matrix (Array2D.init 4 4 (fun j i -> if i = j then 1.0 else 0.0))

let rec matrix = function
    | Translate (Vector (x, y, z)) ->
        Matrix <| Array2D.init 4 4 (fun j i -> if i = j then 1.0 elif i < 3 then 0.0 elif j = 0 then x elif j = 1 then y else z)
    | Scale (x, y, z) ->
        Matrix <| Array2D.init 4 4 (fun j i -> if i <> j then 0.0 elif i = 0 then x elif i = 1 then y elif i = 2 then z else 1.0)
    | Rotate (Vector (ux, uy, uz), angle) ->
        let c = cos (angle / 1.0<rad>)
        let invc = 1.0 - c
        let s = sin (angle / 1.0<rad>)
        [|
            [| c + invc * ux * ux      ; invc * ux * uy - s * uz ; invc * ux * uz + s * uy ; 0.0 |];
            [| invc * ux * uy + s * uz ; c + invc * uy * uy      ; invc * uy * uz - s * ux ; 0.0 |];
            [| invc * ux * uz - s * uy ; invc * uy * uz + s * ux ; c + invc * uz * uz      ; 0.0 |];
            [| 0.0                     ; 0.0                     ; 0.0                     ; 1.0 |];
        |] |> array2D |> Matrix
    | Composed ts ->
        List.foldBack (*) (ts |> List.rev |> List.map matrix) identity

let transpose (Matrix m) =
    Matrix <| Array2D.init 4 4 (fun j i -> Array2D.get m i j)

// The matrix we need to correctly transform normal vectors.
let inverseTranspose =
    inverse >> matrix >> transpose

type private TransformedObject(underlyingObject : Intersectable, transform : Transform) =
    let underlyingObject = underlyingObject
    let modelToWorld = matrix transform
    let worldToModel = matrix (inverse transform)
    let normalToWorld = inverseTranspose transform
    interface Intersectable with
        member this.Intersect r =
            let r' = { o = worldToModel * r.o; d = worldToModel * r.d }
            intersect underlyingObject r' |> Seq.map (fun ix -> { ix with p = modelToWorld * ix.p; n = normalToWorld * ix.n |> normalise })

let transform t o = TransformedObject(o, t)   :> Intersectable
            

            

            

            

            

            

            

            

