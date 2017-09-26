module Light

type LightConfiguration =
| DirectionalLight of Vector
| SoftDirectionalLight of direction:Vector * samples:int * scattering:float

type Light = LightConfiguration * Colour
