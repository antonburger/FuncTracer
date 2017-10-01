module Ray

type Ray = { o: Point; d: Vector }

type RayIntersection = { t: float; p: Point; n: Vector }

type Intersectable = Ray -> RayIntersection seq
