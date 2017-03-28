module OpenSolid.WebGL.Color
    exposing
        ( toVec3
        , toVec4
        )

import Color exposing (Color)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)


scaled : Int -> Float
scaled component =
    toFloat component / 255


toVec3 : Color -> Vec3
toVec3 color =
    let
        { red, green, blue, alpha } =
            Color.toRgb color
    in
        Math.Vector3.vec3 (scaled red) (scaled green) (scaled blue)


toVec4 : Color -> Vec4
toVec4 color =
    let
        { red, green, blue, alpha } =
            Color.toRgb color
    in
        Math.Vector4.vec4 (scaled red) (scaled green) (scaled blue) alpha
