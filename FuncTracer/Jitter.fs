module Jitter

open Vector

type JitterOffset = JitterOffset of x: float * y: float
type Distribution = System.Random -> JitterOffset

// Helper function used by distributions.
let private uniform (random: System.Random) =
    2.0 * random.NextDouble() - 1.0

let square random =
    (uniform random, uniform random) |> JitterOffset

let rec circle random =
    let (x, y) as candidate = uniform random, uniform random
    let outsideCircle = (x * x + y * y) > 1.0
    if outsideCircle then
        circle random
    else
        JitterOffset candidate

let pattern random (distribution: Distribution) sampleCount : JitterOffset list =
    List.init sampleCount (fun _ -> distribution random)

let jitterVector count (maxAngle : float<rad>) vector =
    let random = System.Random()
    let normalised = normalise vector
    let maxOffsetMagnitude = tan (maxAngle/2.0<rad>)
    // Choose a vector not parallel to the direction, to generate the first perpendicular vector.
    let generator = if getX normalised > 0.9 then unitY else unitX
    // Generate an orthonormal basis with the direction.
    let i = generator .** normalised |> normalise
    // No need to normalise when i and normalised are both unit and orthogonal.
    let j = i .** normalised
    // Generate a number of vectors in a circular pattern around the direction.
    let samples = pattern random circle count
    samples |>
    List.map (fun (JitterOffset (x, y)) -> normalised + maxOffsetMagnitude * x * i + maxOffsetMagnitude * y * j |> normalise)
