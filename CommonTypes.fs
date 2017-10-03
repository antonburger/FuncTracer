[<AutoOpen>]
module CommonTypes

type Vector = Vector of (float * float * float) with
    static member (+) (Vector (x1, y1, z1), Vector (x2, y2, z2)) =
        Vector (x1 + x2, y1 + y2, z1 + z2)
    static member (*) (Vector (x, y, z), s) =
        Vector (s * x, s * y, s * z)
    static member (*) (s : float, v : Vector) =
        v * s
    static member (~-) (Vector (x, y, z)) =
        Vector (-x, -y, -z)
    static member (-) (v1 : Vector, v2 : Vector) =
        v1 + -v2
    static member (.*) (Vector (x1, y1, z1), Vector (x2, y2, z2)) =
        x1 * x2 + y1 * y2 + z1 * z2
    static member (.**) (Vector (x1, y1, z1), Vector (x2, y2, z2)) =
        Vector (y1 * z2 - z1 * y2, x2 * z1 - z2 * x1, x1 * y2 - y1 * x2)
    member this.Length = this .* this |> sqrt

type Point = Point of (float * float * float) with
    static member (+) (Point (px, py, pz), Vector (vx, vy, vz)) =
        Point (px + vx, py + vy, pz + vz)
    static member (+) (v : Vector, p : Point) =
        p + v
    static member (-) (Point (x1, y1, z1), Point (x2, y2, z2)) =
        Vector (x1 - x2, y1 - y2, z1 - z2)

type Colour = Colour of (float * float * float) with
    static member DivideByInt (Colour(r,g,b), i:int) = Colour(r/(float)i, g/(float)i, b/(float)i) 
    static member (+) (Colour (r1, g1, b1), Colour (r2, g2, b2)) =
        Colour (r1 + r2, g1 + g2, b1 + b2)
    static member (*) (Colour (r1, g1, b1), Colour (r2, g2, b2)) =
        Colour (r1 * r2, g1 * g2, b1 * b2)
    static member Zero = Colour(0.0,0.0,0.0) 



[<Measure>] type rad
[<Measure>] type deg

module Vector =
    let normalise (v : Vector) =
        let l = v.Length
        if l < 0.0000001
            then v
            else (1.0/l) * v

    let toPoint (Vector (x, y, z)) =
        Point (x, y, z)


    let jitter count (maxAngle : float<rad>) vector =  
        let random = System.Random()
        let normalised = normalise vector
        let maxOffsetMagnitude = tan (maxAngle/2.0<rad>)
        // Just going to hope the random vector isn't parallel
        let randomPerpendicularVector v = Vector(random.NextDouble()-0.5, random.NextDouble()-0.5, random.NextDouble()-0.5).**v |> normalise
        List.init count (fun _ -> 
            let offsetMagnitude = random.NextDouble()*maxOffsetMagnitude
            normalised+offsetMagnitude*(randomPerpendicularVector normalised) |> normalise
        )

    let reflect (n:Vector) (v:Vector) = v-(2.0*(v.*n)*n)


module Point =
    let toVector (Point (x, y, z)) =
        Vector (x, y, z)

module Colour =
    let inline fromRGB r g b = Colour (float r, float g, float b)
    let inline greyScale g = let c = float g in Colour (c, c, c)
    let map f (Colour (r, g, b)) = Colour (f r, f g, f b)

    let black = fromRGB 0 0 0
    let white = fromRGB 1 1 1
    let red = fromRGB 1 0 0
    let blue = fromRGB 0 0 1

module Deg =
    let toRad (d : float<deg>) : float<rad> =
        d * 1.0<rad/deg> * (System.Math.PI / 180.0)

module Rad =
    let toDeg (r : float<rad>) : float<deg> =
        r * 1.0<deg/rad> * (180.0 / System.Math.PI)
