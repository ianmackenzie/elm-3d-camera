module LineSegment3d.Projection exposing (toScreenSpace)

{-|

@docs toScreenSpace

-}

import Camera3d exposing (Camera3d)
import Camera3d.Internal exposing (unsafeProjection)
import Camera3d.Types as Types
import Frame3d
import LineSegment2d exposing (LineSegment2d)
import LineSegment3d exposing (LineSegment3d)
import Plane3d
import Point3d
import Point3d.Projection as Point3d
import Quantity exposing (zero)
import Rectangle2d exposing (Rectangle2d)


{-| Convert a line segment from 3D world to 2D screen coordinates.
-}
toScreenSpace :
    Camera3d worldUnits worldCoordinates
    -> Rectangle2d screenUnits screenCoordinates
    -> LineSegment3d worldUnits worldCoordinates
    -> Maybe (LineSegment2d screenUnits screenCoordinates)
toScreenSpace camera screen lineSegment =
    let
        ( p1, p2 ) =
            LineSegment3d.endpoints lineSegment

        clipPlane =
            Camera3d.clipPlane camera

        candidates =
            [ Point3d.toScreenSpace camera screen p1
            , LineSegment3d.intersectionWithPlane clipPlane lineSegment
                |> Maybe.map (unsafeProjection camera screen)
            , Point3d.toScreenSpace camera screen p2
            ]
    in
    case List.filterMap identity candidates of
        [ firstPoint, secondPoint ] ->
            -- Could be any of:
            --   - start point, end point (no clipping)
            --   - start point, intersection point (end point is clipped)
            --   - intersection point, end point (start point is clipped)
            Just (LineSegment2d.from firstPoint secondPoint)

        [ startPoint, intersectionPoint, endPoint ] ->
            -- Intersection point must be equal to one of either start or end
            -- (to within numerical roundoff), so just return full segment
            Just (LineSegment2d.from startPoint endPoint)

        _ ->
            Nothing
