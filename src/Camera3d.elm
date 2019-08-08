module Camera3d exposing
    ( Camera3d
    , perspective, orthographic
    , viewpoint, clipDepth, clipPlane
    , ray
    , viewMatrix, modelViewMatrix, projectionParameters
    )

{-| A `Camera3d` is a perspective or orthographic camera in 3D, encapsulating
the camera's viewpoint and projection matrix as well as the dimensions of the
screen the camera renders to. This module contains functions for:

  - Defining perspective and orthographic cameras
  - Obtaining view and projection matrices for a given camera, which can then be
    used for WebGL rendering
  - Using cameras to project 3D geometry to 2D screen space

@docs Camera3d


# Constructors

Cameras have some commmon properties regardless of how they are constructed:

  - `viewpoint` defines the position and orientation of the camera in 3D space.
  - `nearClipDistance` and `farClipDistance` specify the standard near and far
    clipping planes used when rendering.
  - `screenWidth` and `screenHeight` specify the dimensions in pixels of the
    screen that will be rendered to (the dimensions of an actual WebGl element,
    for example). This is used to determine aspect ratio when constructing the
    camera's projection matrix, but also by functions such as `Camera3d.point2d`
    which convert from 3D world space to 2D screen space for a given camera.

@docs perspective, orthographic


# Properties

@docs viewpoint, clipDepth, clipPlane


# Ray casting

@docs ray


# WebGL rendering

@docs viewMatrix, modelViewMatrix, projectionParameters

-}

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Camera3d.Types as Types
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Math.Matrix4 exposing (Mat4)
import Math.Vector4 exposing (Vec4)
import Plane3d exposing (Plane3d)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity(..), zero)
import Rectangle2d exposing (Rectangle2d)
import Vector3d exposing (Vector3d)
import Viewpoint3d exposing (Viewpoint3d)


{-| -}
type alias Camera3d units coordinates =
    Types.Camera3d units coordinates


offsetClipPlane : Viewpoint3d units coordinates -> Quantity Float units -> Plane3d units coordinates
offsetClipPlane (Types.Viewpoint3d frame) depth =
    Plane3d.through
        (Point3d.along (Frame3d.zAxis frame) (Quantity.negate depth))
        (Direction3d.reverse (Frame3d.zDirection frame))


{-| Create a perspective camera with the common camera properties plus vertical
field of view given in radians. The horizontal field of view will be chosen to
match the aspect ratio of the screen given by `screenWidth` and `screenHeight`.

    perspectiveCamera =
        Camera3d.perspective
            { viewpoint = cameraViewpoint
            , verticalFieldOfView = degrees 30
            , nearClipDistance = 0.1
            , farClipDistance = 1000
            , screenWidth = 1024
            , screenHeight = 768
            }

-}
perspective :
    { viewpoint : Viewpoint3d units coordinates
    , clipDepth : Quantity Float units
    , verticalFieldOfView : Angle
    }
    -> Camera3d units coordinates
perspective arguments =
    let
        halfFieldOfView =
            Quantity.half (Quantity.abs arguments.verticalFieldOfView)

        frustumSlope =
            Angle.tan halfFieldOfView

        absoluteClipDepth =
            Quantity.abs arguments.clipDepth
    in
    Types.Camera3d
        { viewpoint = arguments.viewpoint
        , clipDepth = absoluteClipDepth
        , clipPlane = offsetClipPlane arguments.viewpoint absoluteClipDepth
        , projection = Types.Perspective frustumSlope
        }


{-| Create an orthographic camera with the common camera properties plus the
height of the orthographic viewport: this is the height in 3D world units of the
section of the model to be rendered. (The width will be chosen to match the
aspect ratio of the screen given by `screenWidth` and `screenHeight`.)

    orthographicCamera =
        Camera3d.orthographic
            { viewpoint = cameraViewpoint
            , viewportHeight = 5
            , nearClipDistance = 0.1
            , farClipDistance = 1000
            , screenWidth = 1024
            , screenHeight = 768
            }

-}
orthographic :
    { viewpoint : Viewpoint3d units coordinates
    , clipDepth : Quantity Float units
    , viewportHeight : Quantity Float units
    }
    -> Camera3d units coordinates
orthographic arguments =
    let
        absoluteClipDepth =
            Quantity.abs arguments.clipDepth
    in
    Types.Camera3d
        { viewpoint = arguments.viewpoint
        , clipDepth = absoluteClipDepth
        , clipPlane = offsetClipPlane arguments.viewpoint absoluteClipDepth
        , projection =
            Types.Orthographic (Quantity.abs arguments.viewportHeight)
        }


{-| Get the viewpoint defining the position and orientation of a camera.
-}
viewpoint : Camera3d units coordinates -> Viewpoint3d units coordinates
viewpoint (Types.Camera3d camera) =
    camera.viewpoint


{-| Get the clip depth of a camera.
-}
clipDepth : Camera3d units coordinates -> Quantity Float units
clipDepth (Types.Camera3d camera) =
    camera.clipDepth


{-| Get the clip plane of a camera.
-}
clipPlane : Camera3d units coordinates -> Plane3d units coordinates
clipPlane (Types.Camera3d camera) =
    camera.clipPlane


{-| Given a camera and a 2D screen point, calculate the corresponding 3D ray as
an `Axis3d`. Conceptually, the ray will pass through the given point on the
screen and will have direction equal to the viewing direction at that point.

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
ray (Types.Camera3d camera) screen point =
    let
        (Types.Viewpoint3d viewpointFrame) =
            camera.viewpoint

        ( screenWidth, screenHeight ) =
            Rectangle2d.dimensions screen

        screenX =
            Point2d.xCoordinateIn (Rectangle2d.axes screen) point

        screenY =
            Point2d.yCoordinateIn (Rectangle2d.axes screen) point
    in
    case camera.projection of
        Types.Perspective frustumSlope ->
            let
                screenZ =
                    Quantity.multiplyBy 0.5 screenHeight
                        |> Quantity.divideBy frustumSlope
                        |> Quantity.negate

                direction =
                    Vector3d.xyz screenX screenY screenZ
                        |> Vector3d.direction
                        |> Maybe.withDefault Direction3d.negativeZ
                        |> Direction3d.placeIn viewpointFrame
            in
            Axis3d.through (Viewpoint3d.eyePoint camera.viewpoint) direction

        Types.Orthographic viewpointHeight ->
            let
                resolution =
                    viewpointHeight |> Quantity.per screenHeight

                origin =
                    Point3d.xyzIn viewpointFrame
                        (screenX |> Quantity.at resolution)
                        (screenY |> Quantity.at resolution)
                        zero
            in
            Axis3d.through origin (Viewpoint3d.viewDirection camera.viewpoint)


{-| Get the [view matrix](http://www.opengl-tutorial.org/beginners-tutorials/tutorial-3-matrices/#the-view-matrix)
of a camera;

    Camera3d.viewMatrix camera

is shorthand for

    Viewpoint3d.viewMatrix (Camera3d.viewpoint camera)

-}
viewMatrix : Camera3d units coordinates -> Mat4
viewMatrix camera =
    Viewpoint3d.viewMatrix (viewpoint camera)


{-| Construct a WebGL model-view matrix given a camera and a `Frame3d` that
defines the position and orientation of an object;

    Camera3d.modelViewMatrix modelFrame camera

is shorthand for

    Viewpoint3d.modelViewMatrix
        modelFrame
        (Camera3d.viewpoint camera)

-}
modelViewMatrix : Frame3d units coordinates defines -> Camera3d units coordinates -> Mat4
modelViewMatrix modelFrame camera =
    Viewpoint3d.modelViewMatrix modelFrame (viewpoint camera)


{-| TODO
-}
projectionParameters : { screenAspectRatio : Float } -> Camera3d units coordinates -> Vec4
projectionParameters { screenAspectRatio } (Types.Camera3d camera) =
    let
        (Quantity n) =
            camera.clipDepth
    in
    case camera.projection of
        Types.Perspective frustumSlope ->
            Math.Vector4.vec4
                n
                screenAspectRatio
                (1 / frustumSlope)
                0

        Types.Orthographic (Quantity viewportHeight) ->
            Math.Vector4.vec4
                n
                screenAspectRatio
                0
                (-2 / viewportHeight)
