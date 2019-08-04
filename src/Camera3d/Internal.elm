module Camera3d.Internal exposing (unsafeProjection)

import Camera3d exposing (Camera3d)
import Camera3d.Types as Types
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity
import Rectangle2d exposing (Rectangle2d)


unsafeProjection :
    Camera3d worldUnits worldCoordinates
    -> Rectangle2d screenUnits screenCoordinates
    -> Point3d worldUnits worldCoordinates
    -> Point2d screenUnits screenCoordinates
unsafeProjection (Types.Camera3d camera) screen point =
    let
        (Types.Viewpoint3d viewpointFrame) =
            camera.viewpoint

        viewX =
            Point3d.xCoordinateIn viewpointFrame point

        viewY =
            Point3d.yCoordinateIn viewpointFrame point

        viewZ =
            Point3d.zCoordinateIn viewpointFrame point

        depth =
            Quantity.negate viewZ

        ( screenWidth, screenHeight ) =
            Rectangle2d.dimensions screen

        aspectRatio =
            Quantity.ratio screenWidth screenHeight
    in
    case camera.projection of
        Types.Perspective frustumSlope ->
            let
                ndcY =
                    Quantity.ratio viewY depth / frustumSlope

                ndcX =
                    Quantity.ratio viewX depth
                        / (aspectRatio * frustumSlope)
            in
            Point2d.xyIn (Rectangle2d.axes screen)
                (Quantity.multiplyBy (ndcX / 2) screenWidth)
                (Quantity.multiplyBy (ndcY / 2) screenHeight)

        Types.Orthographic viewportHeight ->
            let
                halfNdcY =
                    Quantity.ratio viewY viewportHeight

                halfNdcX =
                    Quantity.ratio viewX
                        (Quantity.multiplyBy aspectRatio viewportHeight)
            in
            Point2d.xyIn (Rectangle2d.axes screen)
                (Quantity.multiplyBy halfNdcX screenWidth)
                (Quantity.multiplyBy halfNdcY screenHeight)
