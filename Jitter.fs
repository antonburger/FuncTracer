module Jitter

type JitterOffset = JitterOffset of x: float * y: float
type Distribution = System.Random -> JitterOffset

// Helper function used by distributions.
let private uniform (random: System.Random) =
    random.NextDouble() - 0.5

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
