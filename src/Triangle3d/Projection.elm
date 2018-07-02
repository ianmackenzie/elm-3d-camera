module Triangle3d.Projection exposing (toScreenSpace)

{-|

@docs toScreenSpace

-}

import Camera3d exposing (Camera3d)
import Point3d.Projection as Point3d
import Triangle2d exposing (Triangle2d)
import Triangle3d exposing (Triangle3d)


{-| Convert a triangle from 3D space to 2D screen (pixel) coordinates. The
result will be in a coordinate system where (0,0) is the bottom left of the
screen.
-}
toScreenSpace : Camera3d -> Triangle3d -> Triangle2d
toScreenSpace camera triangle =
    let
        ( p1, p2, p3 ) =
            Triangle3d.vertices triangle
    in
    Triangle2d.fromVertices
        ( Point3d.toScreenSpace camera p1
        , Point3d.toScreenSpace camera p2
        , Point3d.toScreenSpace camera p3
        )
