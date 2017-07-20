module OpenSolid.WebGL.Polyline3d
    exposing
        ( toScreenSpace
        , vertexPositions
        )

import Math.Vector3 exposing (Vec3)
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Polyline3d as Polyline3d
import OpenSolid.WebGL.Camera exposing (Camera)
import OpenSolid.WebGL.Point3d as Point3d


vertexPositions : Polyline3d -> List { position : Vec3 }
vertexPositions polyline =
    let
        toPositionAttribute point =
            { position = Point3d.toVec3 point }
    in
    List.map toPositionAttribute (Polyline3d.vertices polyline)


toScreenSpace : Camera -> Polyline3d -> Polyline2d
toScreenSpace camera polyline =
    Polyline3d.vertices polyline
        |> List.map (Point3d.toScreenSpace camera)
        |> Polyline2d
