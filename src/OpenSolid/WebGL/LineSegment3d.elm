module OpenSolid.WebGL.LineSegment3d
    exposing
        ( toScreenSpace
        )

import Math.Vector3 exposing (Vec3)
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.LineSegment3d as LineSegment3d
import OpenSolid.WebGL.Camera exposing (Camera)
import OpenSolid.WebGL.Point3d as Point3d


toScreenSpace : Camera -> LineSegment3d -> LineSegment2d
toScreenSpace camera lineSegment =
    let
        ( p1, p2 ) =
            LineSegment3d.endpoints lineSegment
    in
    LineSegment2d
        ( Point3d.toScreenSpace camera p1
        , Point3d.toScreenSpace camera p2
        )
