module OpenSolid.WebGL.Point3d exposing (toScreenSpace)

{-|

@docs toScreenSpace

-}

import OpenSolid.Interop.LinearAlgebra.Point3d as Point3d
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.WebGL.Camera as Camera exposing (Camera)


{-| Convert a point from 3D space to 2D screen (pixel) coordinates. The result
will be in a coordinate system where (0,0) is the bottom left of the screen.
-}
toScreenSpace : Camera -> Point3d -> Point2d
toScreenSpace camera point =
    let
        projectionMatrix =
            Camera.projectionMatrix camera

        cameraFrame =
            Camera.frame camera

        viewSpacePoint =
            Point3d.relativeTo cameraFrame point

        ndcPoint =
            Point3d.transformBy projectionMatrix viewSpacePoint

        ( ndcX, ndcY, _ ) =
            Point3d.coordinates ndcPoint

        halfWidth =
            0.5 * Camera.screenWidth camera

        halfHeight =
            0.5 * Camera.screenHeight camera
    in
    Point2d.withCoordinates
        ( halfWidth + halfWidth * ndcX
        , halfHeight + halfHeight * ndcY
        )
