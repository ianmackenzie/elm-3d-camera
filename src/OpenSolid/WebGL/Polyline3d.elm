module OpenSolid.WebGL.Polyline3d
    exposing
        ( toScreenSpace
        , vertexPositions
        )

import Math.Vector3 exposing (Vec3)
import OpenSolid.Interop.LinearAlgebra.Point3d as Point3d
import OpenSolid.Polyline2d as Polyline2d exposing (Polyline2d)
import OpenSolid.Polyline3d as Polyline3d exposing (Polyline3d)
import OpenSolid.WebGL.Camera exposing (Camera)
import OpenSolid.WebGL.Point3d as Point3d


toScreenSpace : Camera -> Polyline3d -> Polyline2d
toScreenSpace camera polyline =
    Polyline3d.vertices polyline
        |> List.map (Point3d.toScreenSpace camera)
        |> Polyline2d.withVertices
