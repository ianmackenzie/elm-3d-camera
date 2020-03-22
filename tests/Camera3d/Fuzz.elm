module Camera3d.Fuzz exposing (ScreenCoordinates, WorldCoordinates, camera, screen, viewpoint)

import Angle
import Camera3d exposing (Camera3d)
import Camera3d.Types as Types
import Fuzz exposing (Fuzzer)
import Geometry.Fuzz as Fuzz
import Length exposing (Meters)
import Pixels exposing (Pixels)
import Point2d
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Rectangle2d exposing (Rectangle2d)
import Viewpoint3d exposing (Viewpoint3d)


type WorldCoordinates
    = WorldCoordinates


viewpoint : Fuzzer (Viewpoint3d Meters WorldCoordinates)
viewpoint =
    Fuzz.map Types.Viewpoint3d Fuzz.frame3d


camera : Fuzzer (Camera3d Meters WorldCoordinates)
camera =
    Fuzz.oneOf
        [ Fuzz.map2
            (\viewpoint_ verticalFieldOfView ->
                Camera3d.perspective
                    { viewpoint = viewpoint_
                    , verticalFieldOfView = verticalFieldOfView
                    }
            )
            viewpoint
            (Fuzz.quantityRange (Angle.degrees 10) (Angle.degrees 90))
        , Fuzz.map2
            (\viewpoint_ viewportHeight ->
                Camera3d.orthographic
                    { viewpoint = viewpoint_
                    , viewportHeight = viewportHeight
                    }
            )
            viewpoint
            (Fuzz.quantityRange (Length.centimeters 10) (Length.meters 5))
        ]


type ScreenCoordinates
    = ScreenCoordinates


screenDimension : Fuzzer (Quantity Float Pixels)
screenDimension =
    Fuzz.quantityRange (Pixels.pixels 100) (Pixels.pixels 1000)


screen : Fuzzer (Rectangle2d Pixels ScreenCoordinates)
screen =
    Fuzz.map2
        (\width height ->
            Rectangle2d.from Point2d.origin (Point2d.xy width height)
        )
        screenDimension
        screenDimension
