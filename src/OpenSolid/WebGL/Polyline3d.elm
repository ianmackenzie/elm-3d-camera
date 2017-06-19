module OpenSolid.WebGL.Polyline3d
    exposing
        ( vertexPositions
        )

import Math.Vector3 exposing (Vec3)
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Polyline3d as Polyline3d
import OpenSolid.WebGL.Point3d as Point3d


vertexPositions : Polyline3d -> List { vertexPosition : Vec3 }
vertexPositions polyline =
    List.map Point3d.toVertexPosition (Polyline3d.vertices polyline)
