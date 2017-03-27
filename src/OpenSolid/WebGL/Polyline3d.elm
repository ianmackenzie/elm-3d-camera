module OpenSolid.WebGL.Polyline3d
    exposing
        ( mesh
        , meshWith
        )

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Polyline3d as Polyline3d
import OpenSolid.WebGL.Point3d as Point3d
import WebGL exposing (Mesh)
import Math.Vector3 exposing (Vec3)


mesh : Polyline3d -> Mesh { vertexPosition : Vec3 }
mesh =
    meshWith (\point -> { vertexPosition = Point3d.toVec3 point })


meshWith : (Point3d -> a) -> Polyline3d -> Mesh a
meshWith attributes =
    WebGL.lineStrip << List.map attributes << Polyline3d.vertices
