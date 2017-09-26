module Light

type LightConfiguration =
| DirectionalLight of Vector

type Light = LightConfiguration * Colour
