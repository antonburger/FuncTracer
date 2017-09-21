module Scene

open Ray

type Scene = Scene of Intersectable list

let intersectScene (Scene objects) r =
    let flip f = fun x y -> f y x
    objects |> Seq.collect (flip intersect r) |> closest
