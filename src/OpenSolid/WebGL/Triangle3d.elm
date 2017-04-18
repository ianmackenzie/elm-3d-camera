module OpenSolid.WebGL.Triangle3d
    exposing
        ( vertexPositions
        , vertexPositionsAndNormals
        )

import OpenSolid.WebGL.Types exposing (..)
import OpenSolid.WebGL.Point3d as Point3d
import OpenSolid.WebGL.Direction3d as Direction3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Triangle3d as Triangle3d
import Math.Vector3 as Vector3


vertexPositions : Triangle3d -> ( VertexPosition, VertexPosition, VertexPosition )
vertexPositions triangle =
    let
        ( p1, p2, p3 ) =
            Triangle3d.vertices triangle
    in
        ( Point3d.toVertexPosition p1
        , Point3d.toVertexPosition p2
        , Point3d.toVertexPosition p3
        )


vertexPositionsAndNormals : Triangle3d -> ( VertexPositionAnd VertexNormal, VertexPositionAnd VertexNormal, VertexPositionAnd VertexNormal )
vertexPositionsAndNormals triangle =
    let
        normalVector =
            case Triangle3d.normalDirection triangle of
                Just direction ->
                    Direction3d.toVec3 direction

                Nothing ->
                    Vector3.vec3 0 0 0

        ( p1, p2, p3 ) =
            Triangle3d.vertices triangle
    in
        ( { vertexPosition = Point3d.toVec3 p1, vertexNormal = normalVector }
        , { vertexPosition = Point3d.toVec3 p2, vertexNormal = normalVector }
        , { vertexPosition = Point3d.toVec3 p3, vertexNormal = normalVector }
        )
