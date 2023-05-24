module Camera3d exposing
    ( Camera3d, Projection(..), FieldOfView, angle, height
    , lookAt, orbit, orbitZ, isometric, isometricElevation, with
    , eyePoint, viewDirection, viewPlane, frame, focalDistance, projection, fovAngle, fovHeight
    , ray
    , frustumSlope
    )

{-| A `Camera3d` is a perspective or orthographic camera in 3D space. This module contains functions
for:

  - Constructing cameras in various ways
  - Extracting properties such as the view direction and view plane of the camera
  - Constructing 3D 'pick rays' based on a given camera and 2D click position

@docs Camera3d, Projection, FieldOfView, angle, height


# Constructors

@docs lookAt, orbit, orbitZ, isometric, isometricElevation, with


# Properties

@docs eyePoint, viewDirection, viewPlane, frame, focalDistance, projection, fovAngle, fovHeight


# Ray casting

@docs ray


# Advanced

@docs frustumSlope

-}

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Rectangle2d exposing (Rectangle2d)
import SketchPlane3d exposing (SketchPlane3d)
import Vector3d


{-| A camera in 3D space, using particular units
(usually [meters](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Length#Meters))
within a particular [coordinate system](https://github.com/ianmackenzie/elm-geometry#coordinate-systems)
(generally a user-defined type like `WorldCoordinates`).
-}
type Camera3d units coordinates
    = Camera3d
        { frame : Frame3d units coordinates { defines : () }
        , focalDistance : Quantity Float units
        , projection : Projection
        , fovAngle : Angle
        }


{-| Defines whether a camera uses [perspective or orthographic projection](https://en.wikipedia.org/wiki/3D_projection).
-}
type Projection
    = Perspective
    | Orthographic


{-| Defines the vertical [field of view](https://en.wikipedia.org/wiki/Field_of_view) of a camera.
Strictly speaking, for a perspective camera the field of view should be defined as an angle and
for an orthographic camera it should be defined as a height. Practically speaking, however, it is
usually possible (and often useful) to convert between the two.

Given a focal distance it is possible (with a bit of trigonometry) to compute the field of view
angle from the field of view height, or vice versa:

![Field of view](https://ianmackenzie.github.io/elm-3d-camera/4.0.0/fov.png)

All the camera construction functions in this module either compute the focal distance
automatically (e.g. as the distance from an eye point to a focal point), or (in the case of the
[`with`](#with) constructor) require you to provide one explicitly.

As a result, this module allows you to specify field of view as either an angle or a height for
either perspective or orthographic cameras, and any necessary conversions will be done
automatically.

-}
type FieldOfView units
    = Angle Angle
    | Height (Quantity Float units)


{-| Specify vertical field of view as an angle. For an orthographic camera, this will be converted
to a height at the camera's focal distance.
-}
angle : Angle -> FieldOfView units
angle givenAngle =
    Angle givenAngle


{-| Specify vertical field of view as a height. For a perspective camera, this will be converted to
an angle at the camera's focal distance.
-}
height : Quantity Float units -> FieldOfView units
height givenHeight =
    Height givenHeight


{-| Construct a camera directly from:

  - Its view plane, which is a `SketchPlane3d` conceptually [aligned with the screen](#viewPlane)
  - The focal distance of the camera (used to convert between angle-based and height-based field of
    view as necessary). This is generally the distance from the camera's eye point to the object
    being focused on (which is also often the center of rotation for the camera).
  - What kind of projection the camera should use
  - What field of view the camera should use

This corresponds most directly to the internal representation of a `Camera3d` and is the most
flexible, but usually also the most akward to use.

-}
with :
    { viewPlane : SketchPlane3d units coordinates defines
    , focalDistance : Quantity Float units
    , projection : Projection
    , fov : FieldOfView units
    }
    -> Camera3d units coordinates
with given =
    Camera3d
        { frame = SketchPlane3d.toFrame given.viewPlane
        , focalDistance = given.focalDistance
        , projection = given.projection
        , fovAngle =
            case given.fov of
                Angle givenAngle ->
                    givenAngle

                Height givenHeight ->
                    Quantity.twice (Angle.atan2 (Quantity.half givenHeight) given.focalDistance)
        }


{-| Construct a `Camera3d` at the given eye point looking towards the given
focal point, with the given global up direction (which will typically be
`Direction3d.positiveZ` or `Direction3d.positiveY`). The focal distance will
be computed as the distance from the eye point to the focal point. For
example, to construct a camera at the point (10, 0, 5) looking towards the
origin:

    camera =
        Camera3d.lookAt
            { eyePoint = Point3d.meters 10 0 5
            , focalPoint = Point3d.origin
            , upDirection = Direction3d.positiveZ
            , projection = Camera3d.Perspective
            , fov = Camera3d.angle (Angle.degrees 30)
            }

    Camera3d.eyePoint camera
    --> Point3d.meters 10 0 5

    Camera3d.viewDirection camera
    --> Direction3d.xz (Angle.degrees -153.43)

That is likely all you need to know but if you are interested in the details and
corner cases, read on!

The view direction of the returned camera will point from the eye point to
the focal point. The Y direction will be chosen to be as close to the global up
direction as possible (the camera will not have any 'roll') and the X
direction will point to the right.

If the direction from the eye point to the focal point is parallel to the global
up direction (that is, the camera represents looking straight up or straight
down) then the X and Y directions will be chosen arbitrarily.

If the given eye point and focal point are coincident (so that there is no well-
defined view direction), then the returned camera will have its Y direction set
to the global up direction and its X and view directions will be chosen
arbitrarily.

-}
lookAt :
    { eyePoint : Point3d units coordinates
    , focalPoint : Point3d units coordinates
    , upDirection : Direction3d coordinates
    , projection : Projection
    , fov : FieldOfView units
    }
    -> Camera3d units coordinates
lookAt given =
    let
        zVector =
            Vector3d.from given.focalPoint given.eyePoint

        yVector =
            Direction3d.toVector given.upDirection

        xVector =
            yVector |> Vector3d.cross zVector

        computedViewPlane =
            case Direction3d.orthonormalize zVector yVector xVector of
                Just ( normalizedZDirection, normalizedYDirection, normalizedXDirection ) ->
                    SketchPlane3d.unsafe
                        { originPoint = given.eyePoint
                        , xDirection = normalizedXDirection
                        , yDirection = normalizedYDirection
                        }

                Nothing ->
                    case Vector3d.direction zVector of
                        Just zDirection ->
                            -- The view vector must be parallel to the up direction,
                            -- since it is non-zero and therefore otherwise would have
                            -- resulted in a valid orthonormalization; therefore, choose
                            -- an arbitrary 'up' direction that is perpendicular to the
                            -- view direction
                            SketchPlane3d.withNormalDirection zDirection given.eyePoint

                        Nothing ->
                            -- The view vector is zero (the eye point and focal point
                            -- are coincident), so construct an arbitrary frame with the
                            -- given up direction
                            let
                                ( arbitraryZDirection, arbitraryXDirection ) =
                                    Direction3d.perpendicularBasis given.upDirection
                            in
                            SketchPlane3d.unsafe
                                { originPoint = given.eyePoint
                                , xDirection = arbitraryXDirection
                                , yDirection = given.upDirection
                                }
    in
    with
        { viewPlane = computedViewPlane
        , focalDistance = Point3d.distanceFrom given.eyePoint given.focalPoint
        , projection = given.projection
        , fov = given.fov
        }


{-| Construct a `Camera3d` looking at the given focal point, the given
distance away. The direction from the focal point to the eye point is defined by
the given azimuth and elevation angles, which are with respect to the given
ground plane (the position of the ground plane does not matter, only its
orientation). For example,

    Camera3d.orbit
        { focalPoint = Point3d.meters 0 0 1
        , groundPlane = SketchPlane3d.xy
        , azimuth = Angle.degrees 0
        , elevation = Angle.degrees 45
        , distance = Length.meters 10
        , projection = Camera3d.Perspective
        , fov = Camera3d.angle (Angle.degrees 30)
        }

is equivalent to

    Camera3d.lookAt
        { focalPoint = Point3d.meters 0 0 1
        , eyePoint = Point3d.meters 7.071 0 7.071
        , upDirection = Direction3d.z
        , projection = Camera3d.Perspective
        , fov = Camera3d.angle (Angle.degrees 30)
        }

As the name suggests, `Camera3d.orbit` is useful for making orbiting cameras;
you can orbit around the focal point by changing just the azimuth, and rotate
up and down by changing just the elevation.

-}
orbit :
    { focalPoint : Point3d units coordinates
    , groundPlane : SketchPlane3d units coordinates defines
    , azimuth : Angle
    , elevation : Angle
    , distance : Quantity Float units
    , projection : Projection
    , fov : FieldOfView units
    }
    -> Camera3d units coordinates
orbit given =
    let
        computedViewPlane =
            SketchPlane3d.toFrame given.groundPlane
                |> Frame3d.moveTo given.focalPoint
                |> Frame3d.rotateAroundOwn Frame3d.zAxis given.azimuth
                |> Frame3d.rotateAroundOwn Frame3d.yAxis (Quantity.negate given.elevation)
                |> Frame3d.translateAlongOwn Frame3d.xAxis given.distance
                |> Frame3d.yzSketchPlane
    in
    with
        { viewPlane = computedViewPlane
        , focalDistance = given.distance
        , projection = given.projection
        , fov = given.fov
        }


{-| A special case of `orbit` for orbiting around a Z axis through the given
focal point. This corresponds to setting `groundPlane` to `SketchPlane3d.xy`,
so azimuth is measured from the X axis towards the Y axis and elevation is
measured up from the XY plane. Not related to the [classic soft drink](https://en.wikipedia.org/wiki/Orbitz_%28drink%29).
-}
orbitZ :
    { focalPoint : Point3d units coordinates
    , azimuth : Angle
    , elevation : Angle
    , distance : Quantity Float units
    , projection : Projection
    , fov : FieldOfView units
    }
    -> Camera3d units coordinates
orbitZ given =
    orbit
        { focalPoint = given.focalPoint
        , groundPlane = SketchPlane3d.xy
        , azimuth = given.azimuth
        , elevation = given.elevation
        , distance = given.distance
        , projection = given.projection
        , fov = given.fov
        }


{-| Not actually a constructor, but a useful value (approximately 35.26 degrees)
to use when constructing cameras using `orbit` or `orbitZ`: using this as the
`elevation` value will result in an [isometric](#isometric) camera if
`azimuth` is set to 45 degrees. Said another way, this is the elevation angle of
a vector with components (1, 1, 1).
-}
isometricElevation : Angle
isometricElevation =
    Angle.atan2 (Quantity.float 1) (Quantity.float (sqrt 2))


{-| Construct a camera looking at the given focal point, the given distance
away, such that a set of XYZ axes at that point will appear to have:

  - Z straight up
  - X pointing to the left and 30 degrees down
  - Y pointing to the right and 30 degrees down

-}
isometric :
    { focalPoint : Point3d units coordinates
    , distance : Quantity Float units
    , projection : Projection
    , fov : FieldOfView units
    }
    -> Camera3d units coordinates
isometric given =
    orbitZ
        { focalPoint = given.focalPoint
        , azimuth = Angle.degrees 45
        , elevation = isometricElevation
        , distance = given.distance
        , projection = given.projection
        , fov = given.fov
        }


{-| The frame of a camera is a `Frame3d` where:

  - The origin point of the frame is the eye point of the camera
  - The X direction of the frame points to the left
  - The Y direction of the frame points up
  - The Z direction of the frame points out of the screen

-}
frame : Camera3d units coordinates -> Frame3d units coordinates defines
frame (Camera3d camera) =
    Frame3d.copy camera.frame


{-| Get the location of a camera.
-}
eyePoint : Camera3d units coordinates -> Point3d units coordinates
eyePoint camera =
    Frame3d.originPoint (frame camera)


{-| Get the direction that a camera is looking in (the direction 'into the screen').
-}
viewDirection : Camera3d units coordinates -> Direction3d coordinates
viewDirection camera =
    Direction3d.reverse (Frame3d.zDirection (frame camera))


{-| The view plane of a camera is a `SketchPlane3d` where:

  - The origin point of the sketch plane is the eye point of the camera
  - The X direction of the sketch plane points to the left
  - The Y direction of the sketch plane points up
  - The normal direction of the sketch plane points out of the screen

-}
viewPlane : Camera3d units coordinates -> SketchPlane3d units coordinates defines
viewPlane camera =
    Frame3d.xySketchPlane (frame camera)


{-| The focal distance of the camera is used to convert between angle-based and height-based field
of view. This generally corresponds to the distance from the camera to the object currently being
looked at/focused on (which is often also the center of rotation/orbit point for the camera).
-}
focalDistance : Camera3d units coordinates -> Quantity Float units
focalDistance (Camera3d camera) =
    camera.focalDistance


{-| Get the projection type of a camera.
-}
projection : Camera3d units coordinates -> Projection
projection (Camera3d camera) =
    camera.projection


{-| Get a camera's vertical field of view as an angle. If necessary, this will be computed from a
height-based field of view using the camera's focal distance.
-}
fovAngle : Camera3d units coordinates -> Angle
fovAngle (Camera3d camera) =
    camera.fovAngle


{-| Half the [`fovAngle`](#fovAngle), expressed as a slope instead of an angle. This is useful for
various low-level camera operations such as computing projection matrices.
-}
frustumSlope : Camera3d units coordinates -> Float
frustumSlope camera =
    Angle.tan (Quantity.half (fovAngle camera))


{-| Get a camera's vertical field of view as a height. If necessary, this will be computed from an
angle-based field of view using the camera's focal distance.
-}
fovHeight : Camera3d units coordinates -> Quantity Float units
fovHeight camera =
    Quantity.twice (focalDistance camera |> Quantity.multiplyBy (frustumSlope camera))


{-| Given a camera, [a rectangle defining the shape and size of a screen](https://package.elm-lang.org/packages/ianmackenzie/elm-3d-camera/latest/#projection-to-screen-space),
and a 2D point within that screen, calculate the corresponding 3D ray as an `Axis3d`.
Conceptually, the ray will pass through the given point on the screen and will
have direction equal to the viewing direction at that point.

For a perspective camera, the origin of the ray will be constant (always equal
to the camera's eye point) and the direction will vary depending on the 2D
screen point. For an orthographic camera, the direction of the ray will be
constant (the view direction of the camera) but the origin will vary depending
on the 2D screen point.

-}
ray :
    Camera3d units coordinates
    -> Rectangle2d screenUnits screenCoordinates
    -> Point2d screenUnits screenCoordinates
    -> Axis3d units coordinates
ray camera screen point =
    let
        cameraFrame =
            frame camera

        ( _, screenHeight ) =
            Rectangle2d.dimensions screen

        screenX =
            Point2d.xCoordinateIn (Rectangle2d.axes screen) point

        screenY =
            Point2d.yCoordinateIn (Rectangle2d.axes screen) point
    in
    case projection camera of
        Perspective ->
            let
                screenZ =
                    Quantity.half screenHeight
                        |> Quantity.divideBy (frustumSlope camera)
                        |> Quantity.negate

                direction =
                    Vector3d.xyz screenX screenY screenZ
                        |> Vector3d.direction
                        |> Maybe.withDefault Direction3d.negativeZ
                        |> Direction3d.placeIn cameraFrame
            in
            Axis3d.through (eyePoint camera) direction

        Orthographic ->
            let
                resolution =
                    fovHeight camera |> Quantity.per screenHeight

                origin =
                    Point3d.xyzIn cameraFrame
                        (screenX |> Quantity.at resolution)
                        (screenY |> Quantity.at resolution)
                        Quantity.zero
            in
            Axis3d.through origin (viewDirection camera)
