module OpenSolid.WebGL.Direction2d
    exposing
        ( toVec2
        )

{-| @docs toVec2
-}

import OpenSolid.Geometry.Types exposing (..)
import Math.Vector2 exposing (Vec2)


{-| Utility function for converting `Direction2d` values to WebGL `Vec2` values.

    Direction2d.toVec2 Direction2d.x
    --> vec2 1 0

-}
toVec2 : Direction2d -> Vec2
toVec2 (Direction2d ( x, y )) =
    Math.Vector2.vec2 x y
