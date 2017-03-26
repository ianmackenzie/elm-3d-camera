module OpenSolid.WebGL.LineSegment3d
    exposing
        ( mesh
        , meshWith
        )

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.LineSegment3d as LineSegment3d
import OpenSolid.WebGL.Point3d as Point3d
import WebGL exposing (Mesh)
import Math.Vector3 exposing (Vec3)


mesh : List LineSegment3d -> Mesh { position : Vec3 }
mesh =
    meshWith
        (\(LineSegment3d ( p1, p2 )) ->
            ( { position = Point3d.toVec3 p1 }
            , { position = Point3d.toVec3 p2 }
            )
        )


meshWith : (LineSegment3d -> ( a, a )) -> List LineSegment3d -> Mesh a
meshWith attributes =
    WebGL.lines << List.map attributes
