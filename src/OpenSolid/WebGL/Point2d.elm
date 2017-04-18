module OpenSolid.WebGL.Point2d
    exposing
        ( toVec2
        )

{-| @docs toVec2
-}

import OpenSolid.Geometry.Types exposing (..)
import Math.Vector2 exposing (Vec2)


{-| Utility function for converting `Point2d` values to WebGL `Vec2` values.

    Point2d.toVec2 (Point2d ( 2, 3 ))
    --> vec2 2 3

-}
toVec2 : Point2d -> Vec2
toVec2 (Point2d ( x, y )) =
    Math.Vector2.vec2 x y
