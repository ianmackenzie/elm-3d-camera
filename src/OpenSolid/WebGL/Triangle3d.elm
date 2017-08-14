module OpenSolid.WebGL.Triangle3d
    exposing
        ( toScreenSpace
        )

{-|

@docs toScreenSpace

-}

import Math.Vector3 as Vector3 exposing (Vec3)
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Triangle3d as Triangle3d
import OpenSolid.WebGL.Camera exposing (Camera)
import OpenSolid.WebGL.Point3d as Point3d


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
    Triangle2d
        ( Point3d.toScreenSpace camera p1
        , Point3d.toScreenSpace camera p2
        , Point3d.toScreenSpace camera p3
        )
