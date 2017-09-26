module Light

open Vector

type LightConfiguration =
    // Constructors are private so that constructor functions can normalise vectors, etc.
    private
    | DirectionalLight of direction : Vector
    | SoftDirectionalLight of direction:Vector * samples:int * scattering:float

type Light = Light of LightConfiguration * Colour

let directional direction colour =
    Light (normalise direction |> DirectionalLight, colour)

let softDirectional direction samples scattering colour =
    Light (SoftDirectionalLight (direction, samples, scattering), colour)

let (|Directional|SoftDirectional|) lightConfiguration =
    match lightConfiguration with
    | DirectionalLight direction -> Directional direction
    | SoftDirectionalLight (direction, samples, scattering) -> SoftDirectional (direction, samples, scattering)
