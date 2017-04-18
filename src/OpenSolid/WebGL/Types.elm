module OpenSolid.WebGL.Types
    exposing
        ( VertexPosition
        , VertexPositionAnd
        , VertexNormal
        , VertexNormalAnd
        , VertexTextureCoordinates
        , VertexTextureCoordinatesAnd
        )

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)


type alias VertexPosition =
    VertexPositionAnd {}


type alias VertexPositionAnd a =
    { a | vertexPosition : Vec3 }


type alias VertexNormal =
    VertexNormalAnd {}


type alias VertexNormalAnd a =
    { a | vertexNormal : Vec3 }


type alias VertexTextureCoordinates =
    VertexTextureCoordinatesAnd {}


type alias VertexTextureCoordinatesAnd a =
    { a | vertexTextureCoordinates : Vec2 }
