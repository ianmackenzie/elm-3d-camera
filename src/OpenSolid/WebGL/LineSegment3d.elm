module OpenSolid.WebGL.LineSegment3d
    exposing
        ( vertexPositions
        )

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.WebGL.Types exposing (..)
import OpenSolid.WebGL.Point3d as Point3d
import OpenSolid.LineSegment3d as LineSegment3d


vertexPositions : LineSegment3d -> ( VertexPosition, VertexPosition )
vertexPositions lineSegment =
    let
        ( p1, p2 ) =
            LineSegment3d.endpoints lineSegment
    in
        ( Point3d.toVertexPosition p1
        , Point3d.toVertexPosition p2
        )
