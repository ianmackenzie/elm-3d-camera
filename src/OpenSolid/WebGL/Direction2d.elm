module OpenSolid.WebGL.Direction2d exposing (toVec2)

import OpenSolid.Geometry.Types exposing (..)
import Math.Vector2 exposing (Vec2)


toVec2 : Direction2d -> Vec2
toVec2 (Direction2d ( x, y )) =
    Math.Vector2.vec2 x y
