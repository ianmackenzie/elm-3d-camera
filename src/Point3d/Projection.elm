module Point3d.Projection exposing (toScreenSpace)

{-|

@docs toScreenSpace

-}

import Camera3d exposing (Camera3d)
import Camera3d.Types as Types
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)


{-| Convert a point from 3D world space to 2D screen (pixel) coordinates. The
result will be in a coordinate system where (0,0) is the bottom left of the
screen.
-}
toScreenSpace : Camera3d -> Point3d -> Point2d
toScreenSpace (Types.Camera3d camera) point =
    let
        { m11, m12, m13, m14, m21, m22, m23, m24, m41, m42, m43, m44 } =
            camera.viewProjectionRecord

        halfWidth =
            0.5 * camera.screenWidth

        halfHeight =
            0.5 * camera.screenHeight

        ( x, y, z ) =
            Point3d.coordinates point

        w =
            m41 * x + m42 * y + m43 * z + m44

        ndcX =
            (m11 * x + m12 * y + m13 * z + m14) / w

        ndcY =
            (m21 * x + m22 * y + m23 * z + m24) / w
    in
    Point2d.fromCoordinates
        ( halfWidth + halfWidth * ndcX
        , halfHeight + halfHeight * ndcY
        )
