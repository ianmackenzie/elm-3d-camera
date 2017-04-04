module OpenSolid.WebGL.Vector3d
    exposing
        ( toVec3
        , toVec4
        )

{-| Utility functions for converting `Vector3d` values to WebGL types.

@docs toVec3, toVec4
-}

import OpenSolid.Geometry.Types exposing (..)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)


{-| Convert a `Vector3d` to a `Vec3`.

    Vector3d.toVec3 (Vector3d ( 2, 1, 3 ))
    --> vec3 2 1 3
-}
toVec3 : Vector3d -> Vec3
toVec3 (Vector3d ( x, y, z )) =
    Math.Vector3.vec3 x y z


{-| Convert a `Vector3d` to a `Vec4`. The resulting `Vec4` will have a W
component of 0 so that it [is not affected by translation](http://www.opengl-tutorial.org/beginners-tutorials/tutorial-3-matrices/)
when performing matrix transformations.

    Vector3d.toVec4 (Vector3d ( 2, 1, 3 ))
    --> vec4 2 1 3 0
-}
toVec4 : Vector3d -> Vec4
toVec4 (Vector3d ( x, y, z )) =
    Math.Vector4.vec4 x y z 0
