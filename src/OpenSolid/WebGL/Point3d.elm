module OpenSolid.WebGL.Point3d
    exposing
        ( toScreenSpace
        , toVec3
        , toVec4
        )

{-| Utility functions for converting `Point3d` values to WebGL types.

@docs toVec3, toVec4

-}

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.WebGL.Camera as Camera exposing (Camera)


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


toScreenSpace : Camera -> Point3d -> Point2d
toScreenSpace camera point =
    let
        projectionMatrix =
            Camera.projectionMatrix camera

        viewSpacePoint =
            Point3d.relativeTo (Camera.frame camera) point

        normalizedCoordinates =
            Math.Matrix4.transform projectionMatrix (toVec3 viewSpacePoint)

        halfWidth =
            0.5 * Camera.screenWidth camera

        halfHeight =
            0.5 * Camera.screenHeight camera

        x =
            halfWidth + halfWidth * Math.Vector3.getX normalizedCoordinates

        y =
            halfHeight + halfHeight * Math.Vector3.getY normalizedCoordinates
    in
    Point2d ( x, y )
