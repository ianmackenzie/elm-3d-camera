module LineSegment3d.Projection exposing (toScreenSpace)

{-|

@docs toScreenSpace

-}

import Camera3d exposing (Camera3d)
import LineSegment2d exposing (LineSegment2d)
import LineSegment3d exposing (LineSegment3d)
import Point3d.Projection as Point3d


{-| Convert a line segment from 3D space to 2D screen (pixel) coordinates. The
result will be in a coordinate system where (0,0) is the bottom left of the
screen.
-}
toScreenSpace :
    Camera3d worldUnits worldCoordinates screenUnits screenCoordinates
    -> LineSegment3d worldUnits worldCoordinates
    -> LineSegment2d screenUnits screenCoordinates
toScreenSpace camera lineSegment =
    let
        ( p1, p2 ) =
            LineSegment3d.endpoints lineSegment
    in
    LineSegment2d.fromEndpoints
        ( Point3d.toScreenSpace camera p1
        , Point3d.toScreenSpace camera p2
        )
