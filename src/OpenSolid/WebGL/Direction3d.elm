module OpenSolid.WebGL.Direction3d
    exposing
        ( toVec3
        , toVec4
        )

{-| Utility functions for converting `Direction3d` values to WebGL types.

@docs toVec3, toVec4
-}

import OpenSolid.Geometry.Types exposing (..)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)


{-| Convert a `Direction3d` to a `Vec3`.

    Direction3d.toVec3 Direction3d.y
    --> vec3 0 1 0
-}
toVec3 : Direction3d -> Vec3
toVec3 (Direction3d ( x, y, z )) =
    Math.Vector3.vec3 x y z


{-| Convert a `Direction3d` to a `Vec4`. The resulting `Vec4` will have a W
component of 0 so that it [is not affected by translation](http://www.opengl-tutorial.org/beginners-tutorials/tutorial-3-matrices/)
when performing matrix transformations.

    Direction3d.toVec4 Direction3d.y
    --> vec4 0 1 0 0
-}
toVec4 : Direction3d -> Vec4
toVec4 (Direction3d ( x, y, z )) =
    Math.Vector4.vec4 x y z 0
