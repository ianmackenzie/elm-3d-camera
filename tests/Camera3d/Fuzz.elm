module Camera3d.Fuzz exposing (ScreenCoordinates, WorldCoordinates, camera, screen)

import Angle
import Camera3d exposing (Camera3d)
import Fuzz exposing (Fuzzer)
import Geometry.Fuzz as Fuzz
import Length exposing (Meters)
import Pixels exposing (Pixels)
import Point2d
import Quantity exposing (Quantity)
import Rectangle2d exposing (Rectangle2d)


type WorldCoordinates
    = WorldCoordinates


projectionFuzzer : Fuzzer Camera3d.Projection
projectionFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant Camera3d.Perspective
        , Fuzz.constant Camera3d.Orthographic
        ]


fovFuzzer : Fuzzer (Camera3d.FieldOfView Meters)
fovFuzzer =
    Fuzz.oneOf
        [ Fuzz.map Camera3d.angle (Fuzz.quantityRange (Angle.degrees 10) (Angle.degrees 90))
        , Fuzz.map Camera3d.height (Fuzz.quantityRange (Length.centimeters 10) (Length.meters 5))
        ]


camera : Fuzzer (Camera3d Meters WorldCoordinates)
camera =
    Fuzz.map4
        (\viewPlane focalDistance projection fov ->
            Camera3d.with
                { viewPlane = viewPlane
                , focalDistance = focalDistance
                , projection = projection
                , fov = fov
                }
        )
        Fuzz.sketchPlane3d
        (Fuzz.quantityRange (Length.meters 1) (Length.meters 5))
        projectionFuzzer
        fovFuzzer


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
