module Camera3d.Types exposing
    ( Camera3d(..)
    , Projection(..)
    , Viewpoint3d(..)
    )

import Frame3d exposing (Frame3d)
import Quantity exposing (Quantity)


type EyeCoordinates
    = EyeCoordinates


type Viewpoint3d units coordinates
    = Viewpoint3d (Frame3d units coordinates { defines : EyeCoordinates })


type Projection units
    = Perspective Float
    | Orthographic (Quantity Float units)


type Camera3d units coordinates
    = Camera3d
        { viewpoint : Viewpoint3d units coordinates
        , projection : Projection units
        }
