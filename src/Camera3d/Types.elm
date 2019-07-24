module Camera3d.Types exposing
    ( Camera3d(..)
    , Mat4Record
    , Projection(..)
    , Properties
    , Viewpoint3d(..)
    )

import Frame2d exposing (Frame2d)
import Frame3d exposing (Frame3d)
import Math.Matrix4 exposing (Mat4)
import Quantity exposing (Quantity, Rate)
import Rectangle2d exposing (Rectangle2d)


type EyeCoordinates
    = EyeCoordinates


type ViewPlaneCoordinates
    = ViewPlaneCoordinates


type Viewpoint3d units coordinates
    = Viewpoint3d (Frame3d units coordinates { defines : EyeCoordinates })


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


type alias Properties worldUnits worldCoordinates screenUnits screenCoordinates =
    { viewpoint : Viewpoint3d worldUnits worldCoordinates
    , screen : Rectangle2d screenUnits screenCoordinates
    , projectionMatrix : Mat4
    , viewProjectionRecord : Mat4Record
    }


type Projection worldUnits screenUnits
    = Perspective { screenDistance : Quantity Float screenUnits }
    | Orthographic { resolution : Quantity Float (Rate screenUnits worldUnits) }


type Camera3d worldUnits worldCoordinates screenUnits screenCoordinates
    = Camera3d (Properties worldUnits worldCoordinates screenUnits screenCoordinates) (Projection worldUnits screenUnits)
