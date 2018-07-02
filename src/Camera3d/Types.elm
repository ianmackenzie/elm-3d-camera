module Camera3d.Types exposing (..)

import Frame3d exposing (Frame3d)
import Math.Matrix4 exposing (Mat4)


type Viewpoint3d
    = Viewpoint3d Frame3d


type alias Mat4Record =
    { m11 : Float
    , m12 : Float
    , m13 : Float
    , m14 : Float
    , m21 : Float
    , m22 : Float
    , m23 : Float
    , m24 : Float
    , m31 : Float
    , m32 : Float
    , m33 : Float
    , m34 : Float
    , m41 : Float
    , m42 : Float
    , m43 : Float
    , m44 : Float
    }


type Camera3d
    = Camera3d
        { viewpoint : Viewpoint3d
        , projectionMatrix : Mat4
        , screenWidth : Float
        , screenHeight : Float
        , viewProjectionRecord : Mat4Record
        }
