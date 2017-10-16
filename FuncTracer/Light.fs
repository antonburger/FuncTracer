module Light

open Vector

type Falloff = Falloff of constant: float * linear: float * quadratic: float

type LightConfiguration =
    // Constructors are private so that constructor functions can normalise vectors, etc.
    private
    | DirectionalLight of direction : Vector
    | SoftDirectionalLight of direction:Vector * samples:int * scattering:float<rad>
    | PointLight of position: Point * falloff: Falloff

type Light = Light of LightConfiguration * Colour

let attenuate (Falloff (c, l, q)) distance =
    1.0 / (c + distance * (l + distance * q))

let directional direction colour =
    Light (normalise direction |> DirectionalLight, colour)

let softDirectional direction samples scattering colour =
    Light (SoftDirectionalLight (normalise direction, samples, scattering), colour)

let positional position falloff colour =
    Light (PointLight (position, falloff), colour)

let (|Directional|SoftDirectional|Point|) lightConfiguration =
    match lightConfiguration with
    | DirectionalLight direction -> Directional direction
    | SoftDirectionalLight (direction, samples, scattering) -> SoftDirectional (direction, samples, scattering)
    | PointLight (position, falloff) -> Point (position, falloff)
