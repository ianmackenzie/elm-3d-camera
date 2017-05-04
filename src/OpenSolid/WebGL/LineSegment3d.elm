module OpenSolid.WebGL.LineSegment3d
    exposing
        ( vertexPositions
        , toScreenSpace
        )

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.WebGL.Point3d as Point3d
import OpenSolid.WebGL.Camera exposing (Camera)
import OpenSolid.LineSegment3d as LineSegment3d
import Math.Vector3 exposing (Vec3)


vertexPositions : LineSegment3d -> ( { vertexPosition : Vec3 }, { vertexPosition : Vec3 } )
vertexPositions lineSegment =
    let
        ( p1, p2 ) =
            LineSegment3d.endpoints lineSegment
    in
        ( Point3d.toVertexPosition p1
        , Point3d.toVertexPosition p2
        )


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
