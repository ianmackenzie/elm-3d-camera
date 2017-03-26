module OpenSolid.WebGL.Point2d exposing (toVec2)

import OpenSolid.Geometry.Types exposing (..)
import Math.Vector2 exposing (Vec2)


toVec2 : Point2d -> Vec2
toVec2 (Point2d ( x, y )) =
    Math.Vector2.vec2 x y
