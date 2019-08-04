module Point3d.Projection exposing (toScreenSpace)

{-|

@docs toScreenSpace

-}

import Camera3d exposing (Camera3d)
import Camera3d.Internal exposing (unsafeProjection)
import Camera3d.Types as Types
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity exposing (zero)
import Rectangle2d exposing (Rectangle2d)


{-| Convert a point from 3D world to 2D screen coordinates.
-}
toScreenSpace :
    Camera3d worldUnits worldCoordinates
    -> Rectangle2d screenUnits screenCoordinates
    -> Point3d worldUnits worldCoordinates
    -> Maybe (Point2d screenUnits screenCoordinates)
toScreenSpace camera screen point =
    if
        point
            |> Point3d.signedDistanceFrom (Camera3d.clipPlane camera)
            |> Quantity.greaterThanOrEqualTo zero
    then
        Just (unsafeProjection camera screen point)

    else
        Nothing
