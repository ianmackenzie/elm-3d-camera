module OpenSolid.WebGL.Point3d exposing (toScreenSpace)

{-|

@docs toScreenSpace

-}

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.WebGL.Camera as Camera exposing (Camera)


{-| Convert a point from 3D space to 2D screen (pixel) coordinates. The result
will be in a coordinate system where (0,0) is the bottom left of the screen.
-}
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
