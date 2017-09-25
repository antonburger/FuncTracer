module Vector

let zero = Vector (0.0, 0.0, 0.0)
let i = Vector (1.0, 0.0, 0.0)
let j = Vector (0.0, 1.0, 0.0)
let k = Vector (0.0, 0.0, 1.0)

let normalise (v : Vector) =
    let l = v.Length
    if l < 0.0000001
        then v
        else (1.0/l) * v
