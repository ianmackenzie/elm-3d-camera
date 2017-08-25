module OpenSolid.WebGL.Polyline3d exposing (toScreenSpace)

{-|

@docs toScreenSpace

-}

import OpenSolid.Polyline2d as Polyline2d exposing (Polyline2d)
import OpenSolid.Polyline3d as Polyline3d exposing (Polyline3d)
import OpenSolid.WebGL.Camera exposing (Camera)
import OpenSolid.WebGL.Point3d as Point3d


{-| Convert a polyline from 3D space to 2D screen (pixel) coordinates. The
result will be in a coordinate system where (0,0) is the bottom left of the
screen.
-}
toScreenSpace : Camera -> Polyline3d -> Polyline2d
toScreenSpace camera polyline =
    Polyline3d.vertices polyline
        |> List.map (Point3d.toScreenSpace camera)
        |> Polyline2d.withVertices
