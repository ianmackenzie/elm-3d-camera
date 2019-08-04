module Triangle3d.Projection exposing
    ( toScreenSpace
    , isFrontFacing
    )

{-|

@docs toScreenSpace

-}

import Camera3d exposing (Camera3d)
import Camera3d.Internal exposing (unsafeProjection)
import Camera3d.Types as Types
import Frame3d
import LineSegment3d
import Plane3d
import Point3d
import Point3d.Projection as Point3d
import Polygon2d exposing (Polygon2d)
import Quantity exposing (zero)
import Rectangle2d exposing (Rectangle2d)
import Triangle3d exposing (Triangle3d)
import Vector3d
import Viewpoint3d


{-| Convert a triangle from 3D world to 2D screen coordinates.
-}
toScreenSpace :
    Camera3d worldUnits worldCoordinates
    -> Rectangle2d screenUnits screenCoordinates
    -> Triangle3d worldUnits worldCoordinates
    -> Maybe (Polygon2d screenUnits screenCoordinates)
toScreenSpace camera screen triangle =
    let
        ( p1, p2, p3 ) =
            Triangle3d.vertices triangle

        edge12 =
            LineSegment3d.from p1 p2

        edge23 =
            LineSegment3d.from p2 p3

        edge31 =
            LineSegment3d.from p3 p1

        clipPlane =
            Camera3d.clipPlane camera

        candidates =
            [ Point3d.toScreenSpace camera screen p1
            , LineSegment3d.intersectionWithPlane clipPlane edge12
                |> Maybe.map (unsafeProjection camera screen)
            , Point3d.toScreenSpace camera screen p2
            , LineSegment3d.intersectionWithPlane clipPlane edge23
                |> Maybe.map (unsafeProjection camera screen)
            , Point3d.toScreenSpace camera screen p3
            , LineSegment3d.intersectionWithPlane clipPlane edge31
                |> Maybe.map (unsafeProjection camera screen)
            ]
    in
    case List.filterMap identity candidates of
        [] ->
            Nothing

        vertices ->
            Just (Polygon2d.singleLoop vertices)


isFrontFacing : Camera3d units coordinates -> Triangle3d units coordinates -> Bool
isFrontFacing camera triangle =
    case Triangle3d.normalDirection triangle of
        Just normalDirection ->
            let
                viewpoint =
                    Camera3d.viewpoint camera

                eyePoint =
                    Viewpoint3d.eyePoint viewpoint

                ( p1, _, _ ) =
                    Triangle3d.vertices triangle
            in
            Vector3d.from p1 eyePoint
                |> Vector3d.componentIn normalDirection
                |> Quantity.greaterThan zero

        Nothing ->
            False
