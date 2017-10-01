module Math

// Return the solutions to the equation ax^2 + bx + c = 0
let quadratic a b c =
    let discriminant = b ** 2.0 - 4.0 * a * c
    if discriminant < 0.0 then List.empty
    else
        let sq = sqrt discriminant
        let twoa = 2.0 * a
        [ (-b + sq) / twoa; (-b - sq) / twoa ]
