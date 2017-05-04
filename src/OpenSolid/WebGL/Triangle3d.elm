module OpenSolid.WebGL.Triangle3d
    exposing
        ( vertexPositions
        , vertexPositionsAndNormals
        , toScreenSpace
        )

import OpenSolid.WebGL.Point3d as Point3d
import OpenSolid.WebGL.Direction3d as Direction3d
import OpenSolid.WebGL.Camera exposing (Camera)
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Triangle3d as Triangle3d
import Math.Vector3 as Vector3 exposing (Vec3)


vertexPositions : Triangle3d -> ( { vertexPosition : Vec3 }, { vertexPosition : Vec3 }, { vertexPosition : Vec3 } )
vertexPositions triangle =
    let
        ( p1, p2, p3 ) =
            Triangle3d.vertices triangle
    in
        ( Point3d.toVertexPosition p1
        , Point3d.toVertexPosition p2
        , Point3d.toVertexPosition p3
        )


vertexPositionsAndNormals : Triangle3d -> ( { vertexPosition : Vec3, vertexNormal : Vec3 }, { vertexPosition : Vec3, vertexNormal : Vec3 }, { vertexPosition : Vec3, vertexNormal : Vec3 } )
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


toScreenSpace : Camera -> Triangle3d -> Triangle2d
toScreenSpace camera triangle =
    let
        ( p1, p2, p3 ) =
            Triangle3d.vertices triangle
    in
        Triangle2d
            ( Point3d.toScreenSpace camera p1
            , Point3d.toScreenSpace camera p2
            , Point3d.toScreenSpace camera p3
            )
