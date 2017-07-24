module OpenSolid.WebGL.Triangle3d
    exposing
        ( toScreenSpace
        , vertexPositions
        , vertexPositionsAndNormals
        )

import Math.Vector3 as Vector3 exposing (Vec3)
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Triangle3d as Triangle3d
import OpenSolid.WebGL.Camera exposing (Camera)
import OpenSolid.WebGL.Direction3d as Direction3d
import OpenSolid.WebGL.Point3d as Point3d


vertexPositions : Triangle3d -> ( Vec3, Vec3, Vec3 )
vertexPositions triangle =
    let
        ( p1, p2, p3 ) =
            Triangle3d.vertices triangle
    in
    ( Point3d.toVec3 p1, Point3d.toVec3 p2, Point3d.toVec3 p3 )


vertexPositionsAndNormals : Triangle3d -> ( ( Vec3, Vec3 ), ( Vec3, Vec3 ), ( Vec3, Vec3 ) )
vertexPositionsAndNormals triangle =
    let
        ( p1, p2, p3 ) =
            Triangle3d.vertices triangle

        normalVector =
            Triangle3d.normalDirection triangle
                |> Maybe.map Direction3d.toVec3
                |> Maybe.withDefault (Vector3.vec3 0 0 0)
    in
    ( ( Point3d.toVec3 p1, normalVector )
    , ( Point3d.toVec3 p2, normalVector )
    , ( Point3d.toVec3 p3, normalVector )
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
