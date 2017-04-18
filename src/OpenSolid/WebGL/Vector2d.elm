module OpenSolid.WebGL.Vector2d
    exposing
        ( toVec2
        )

{-| @docs toVec2
-}

import OpenSolid.Geometry.Types exposing (..)
import Math.Vector2 exposing (Vec2)


{-| Utility function for converting `Vector2d` values to WebGL `Vec2` values.

    Vector2d.toVec2 (Vector2d ( 2, 3 ))
    --> vec2 2 3

-}
toVec2 : Vector2d -> Vec2
toVec2 (Vector2d ( x, y )) =
    Math.Vector2.vec2 x y
