module OpenSolid.WebGL.Color
    exposing
        ( toVec3
        , toVec4
        )

{-| Utility functions for converting `Color` values to WebGL types. Note that
while `Color` values in Elm are constructed from `Int` RGB components in the
range 0..255 and a `Float` alpha component in the range 0..1, the functions in
this module convert all components to floating-point values in the range 0..1
as expected by OpenGL.

@docs toVec3, toVec4

-}

import Color exposing (Color)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)


scaled : Int -> Float
scaled component =
    toFloat component / 255


{-| Convert a `Color` to a `Vec3` in RGB format, ignoring any alpha value.

    Color.toVec3 (Color.rgb 51 153 102)
    --> vec3 0.2 0.6 0.4

    Color.toVec3 (Color.rgba 51 153 102 0.5)
    --> vec3 0.2 0.6 0.4

-}
toVec3 : Color -> Vec3
toVec3 color =
    let
        { red, green, blue, alpha } =
            Color.toRgb color
    in
        Math.Vector3.vec3 (scaled red) (scaled green) (scaled blue)


{-| Convert a `Color` to a `Vec4` in RGBA format.

    Color.toVec4 (Color.rgb 51 153 102)
    --> vec4 0.2 0.6 0.4 1

    Color.toVec4 (Color.rgba 51 153 102 0.5)
    --> vec4 0.2 0.6 0.4 0.5

-}
toVec4 : Color -> Vec4
toVec4 color =
    let
        { red, green, blue, alpha } =
            Color.toRgb color
    in
        Math.Vector4.vec4 (scaled red) (scaled green) (scaled blue) alpha
