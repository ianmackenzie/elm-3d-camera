module Polyline3d.Projection exposing (toScreenSpace)

{-|

@docs toScreenSpace

-}

import Camera3d exposing (Camera3d)
import Point3d.Projection as Point3d
import Polyline2d exposing (Polyline2d)
import Polyline3d exposing (Polyline3d)


{-| Convert a polyline from 3D space to 2D screen (pixel) coordinates. The
result will be in a coordinate system where (0,0) is the bottom left of the
screen.
-}
toScreenSpace :
    Camera3d worldUnits worldCoordinates screenUnits screenCoordinates
    -> Polyline3d worldUnits worldCoordinates
    -> Polyline2d screenUnits screenCoordinates
toScreenSpace camera polyline =
    Polyline3d.vertices polyline
        |> List.map (Point3d.toScreenSpace camera)
        |> Polyline2d.fromVertices
