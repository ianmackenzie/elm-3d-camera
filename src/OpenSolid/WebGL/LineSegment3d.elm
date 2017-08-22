module OpenSolid.WebGL.LineSegment3d exposing (toScreenSpace)

{-|

@docs toScreenSpace

-}

import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.LineSegment3d as LineSegment3d exposing (LineSegment3d)
import OpenSolid.WebGL.Camera exposing (Camera)
import OpenSolid.WebGL.Point3d as Point3d


{-| Convert a line segment from 3D space to 2D screen (pixel) coordinates. The
result will be in a coordinate system where (0,0) is the bottom left of the
screen.
-}
toScreenSpace : Camera -> LineSegment3d -> LineSegment2d
toScreenSpace camera lineSegment =
    let
        ( p1, p2 ) =
            LineSegment3d.endpoints lineSegment
    in
    LineSegment2d.withEndpoints
        ( Point3d.toScreenSpace camera p1
        , Point3d.toScreenSpace camera p2
        )
