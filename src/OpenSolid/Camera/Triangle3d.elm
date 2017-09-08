module OpenSolid.Camera.Triangle3d exposing (toScreenSpace)

{-|

@docs toScreenSpace

-}

import OpenSolid.Camera exposing (Camera)
import OpenSolid.Camera.Point3d as Point3d
import OpenSolid.Triangle2d as Triangle2d exposing (Triangle2d)
import OpenSolid.Triangle3d as Triangle3d exposing (Triangle3d)


{-| Convert a triangle from 3D space to 2D screen (pixel) coordinates. The
result will be in a coordinate system where (0,0) is the bottom left of the
screen.
-}
toScreenSpace : Camera -> Triangle3d -> Triangle2d
toScreenSpace camera triangle =
    let
        ( p1, p2, p3 ) =
            Triangle3d.vertices triangle
    in
    Triangle2d.withVertices
        ( Point3d.toScreenSpace camera p1
        , Point3d.toScreenSpace camera p2
        , Point3d.toScreenSpace camera p3
        )
