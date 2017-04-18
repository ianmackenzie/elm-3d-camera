module OpenSolid.WebGL.Point3d
    exposing
        ( toVec3
        , toVec4
        , toVertexPosition
        )

{-| Utility functions for converting `Point3d` values to WebGL types.

@docs toVec3, toVec4

-}

import OpenSolid.WebGL.Types exposing (..)
import OpenSolid.Geometry.Types exposing (..)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)


{-| Convert a `Point3d` to a `Vec3`.

    Point3d.toVec3 (Point3d ( 2, 1, 3 ))
    --> vec3 2 1 3

-}
toVec3 : Point3d -> Vec3
toVec3 (Point3d ( x, y, z )) =
    Math.Vector3.vec3 x y z


{-| Convert a `Point3d` to a `Vec4`. The resulting `Vec4` will have a W
component of 1 so that it [is affected by translation](http://www.opengl-tutorial.org/beginners-tutorials/tutorial-3-matrices/)
when performing matrix transformations.

    Point3d.toVec4 (Point3d ( 2, 1, 3 ))
    --> vec4 2 1 3 1

-}
toVec4 : Point3d -> Vec4
toVec4 (Point3d ( x, y, z )) =
    Math.Vector4.vec4 x y z 1


toVertexPosition : Point3d -> VertexPosition
toVertexPosition point =
    { vertexPosition = toVec3 point }
