module Camera3d.Random exposing (ScreenCoordinates, WorldCoordinates, camera, screen)

import Angle
import Camera3d exposing (Camera3d)
import Geometry.Random as Random
import Length exposing (Meters)
import Pixels exposing (Pixels)
import Point2d
import Quantity exposing (Quantity)
import Random exposing (Generator)
import Rectangle2d exposing (Rectangle2d)


type WorldCoordinates
    = WorldCoordinates


projectionGenerator : Generator Camera3d.Projection
projectionGenerator =
    Random.uniform
        (Random.constant Camera3d.Perspective)
        [ Random.constant Camera3d.Orthographic
        ]
        |> Random.andThen identity


fovGenerator : Generator (Camera3d.FieldOfView Meters)
fovGenerator =
    Random.uniform
        (Random.map Camera3d.angle (Random.map Angle.degrees (Random.float 10 90)))
        [ Random.map Camera3d.height (Random.map Length.centimeters (Random.float 10 500))
        ]
        |> Random.andThen identity


camera : Generator (Camera3d Meters WorldCoordinates)
camera =
    Random.map4
        (\viewPlane focalDistance projection fov ->
            Camera3d.with
                { viewPlane = viewPlane
                , focalDistance = focalDistance
                , projection = projection
                , fov = fov
                }
        )
        Random.sketchPlane3d
        (Random.map Length.meters (Random.float 1 5))
        projectionGenerator
        fovGenerator


type ScreenCoordinates
    = ScreenCoordinates


screenDimension : Generator (Quantity Float Pixels)
screenDimension =
    Random.map Pixels.float (Random.float 100 1000)


screen : Generator (Rectangle2d Pixels ScreenCoordinates)
screen =
    Random.map2
        (\width height ->
            Rectangle2d.from Point2d.origin (Point2d.xy width height)
        )
        screenDimension
        screenDimension
