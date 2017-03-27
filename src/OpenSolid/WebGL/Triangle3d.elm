module OpenSolid.WebGL.Triangle3d
    exposing
        ( mesh
        , meshWith
        )

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Triangle3d as Triangle3d
import OpenSolid.WebGL.Point3d as Point3d
import WebGL exposing (Mesh)
import Math.Vector3 exposing (Vec3)


vertexPositionAttribute : Point3d -> { vertexPosition : Vec3 }
vertexPositionAttribute point =
    { vertexPosition = Point3d.toVec3 point }


mesh : List Triangle3d -> Mesh { vertexPosition : Vec3 }
mesh =
    meshWith
        (\(Triangle3d ( p1, p2, p3 )) ->
            ( { vertexPosition = Point3d.toVec3 p1 }
            , { vertexPosition = Point3d.toVec3 p2 }
            , { vertexPosition = Point3d.toVec3 p3 }
            )
        )


meshWith : (Triangle3d -> ( a, a, a )) -> List Triangle3d -> Mesh a
meshWith attributes =
    WebGL.triangles << List.map attributes


fan : List Point3d -> Mesh { vertexPosition : Vec3 }
fan =
    fanWith vertexPositionAttribute


fanWith : (Point3d -> a) -> List Point3d -> Mesh a
fanWith attributes =
    WebGL.triangleFan << List.map attributes


strip : List Point3d -> Mesh { vertexPosition : Vec3 }
strip =
    stripWith vertexPositionAttribute


stripWith : (Point3d -> a) -> List Point3d -> Mesh a
stripWith attributes =
    WebGL.triangleStrip << List.map attributes
