module OpenSolid.WebGL.Camera
    exposing
        ( Camera
        , frame
        , lookAt
        , modelViewMatrix
        , modelViewProjectionMatrix
        , orthographic
        , perspective
        , projectionMatrix
        , screenHeight
        , screenWidth
        , viewMatrix
        )

{-| This module contains functions for defining cameras in 3D and obtaining
WebGL view and projection matrices from them. This provides a convenient way
to construct view and projection matrices which then can be used with any
rendering framework or shaders that you want.

@docs Camera


# Constructors

A camera's position and orientation is defined by a `Frame3d` value where the
orign point of the frame is the 'eye point' of the camera, the positive X
direction of the frame points to the right, and the positive Y direction of the
frame points up. This means that the viewing direction is the _negative_ Z
direction of the frame (positive Z is out of the screen), which is the
convention used by WebGL.

@docs lookAt

Cameras have some commmon properties regardless of how they are constructed:

  - `frame` defines the position and orientation of the camera in 3D space,
    as discussed above
  - `nearClipDistance` and `farClipDistance` specify the standard near and far
    clipping planes used when rendering
  - `screenWidth` and `screenHeight` specify the dimensions in pixels of the
    screen that will be rendered to (the dimensions of the actual WebGl
    element). This is used to determine aspect ratio when constructing the
    camera's projection matrix, but also when using other functions in this
    package such as `Point3d.toScreenSpace` which convert from 3D model space
    to 2D pixel coordinates for a given camera.

@docs perspective, orthographic


# Matrices

@docs viewMatrix, projectionMatrix, modelViewMatrix, modelViewProjectionMatrix


# Accessors

@docs frame, screenHeight, screenWidth

-}

import Math.Matrix4 as Matrix4 exposing (Mat4)
import OpenSolid.Direction3d as Direction3d exposing (Direction3d)
import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
import OpenSolid.Interop.LinearAlgebra.Frame3d as Frame3d
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)


{-| A perspective or orthographic camera in 3D, encapsulating the camera's
position, orientation and projection matrix as well as the dimensions of the
screen the camera renders to.
-}
type Camera
    = Camera
        { frame : Frame3d
        , screenWidth : Float
        , screenHeight : Float
        , projectionMatrix : Mat4
        }


{-| Construct a `Frame3d` that can be used as the camera frame for a camera at
the given eye point looking towards the given focal point, with the given
global up direction (which will typically be `Direction3d.positiveZ` or
`Direction3d.positiveY`). For example, to construct a camera at the point
(10, 0, 5) looking towards the origin:

    cameraFrame =
        Camera.lookAt
            { eyePoint = Point3d.withCoordinates ( 10, 0, 5 )
            , focalPoint = Point3d.origin
            , upDirection = Direction3d.positiveZ
            }

    Frame3d.originPoint cameraFrame
    --> Point3d.withCoordinates ( 10, 0, 5 )

    Frame3d.xDirection cameraFrame
    --> Direction3d.positiveY

    Frame3d.yDirection cameraFrame
    --> Direction3d.with
    -->     { azimuth = degrees 180
    -->     , elevation = degrees 63.43
    -->     }

    Frame3d.zDirection cameraFrame
    --> Direction3d.with
    -->     { azimuth = 0
    -->     , elevation = degrees 26.57
    -->     }

That is likely all you need to know but if you are interested in the details,
read on!

The Z direction of the returned frame will point _from_ the focal point _to_ the
eye point (since as mentioned above WebGL expects the viewing direction to be
the negative Z direction). The Y direction will be chosen to be as close to the
global up direction as possible (the camera will not 'roll') and the X direction
will point to the right.

If the direction from the eye point to the focal point is parallel to the global
up direction (that is, the camera is pointing straight up or straight down) then
the X and Y directions will be chosen arbitrarily.

If the given eye point and focal point are coincident (so that there is no well-
defined viewing direction), then the returned frame will have its Y direction
set to the global up direction and its X and Z directions will be chosen
arbitrarily.

-}
lookAt : { focalPoint : Point3d, eyePoint : Point3d, upDirection : Direction3d } -> Frame3d
lookAt { focalPoint, eyePoint, upDirection } =
    let
        zVector =
            Vector3d.from focalPoint eyePoint

        yVector =
            Direction3d.toVector upDirection

        xVector =
            Vector3d.crossProduct yVector zVector
    in
    case Vector3d.orthonormalize ( zVector, yVector, xVector ) of
        Just ( zDirection, yDirection, xDirection ) ->
            Frame3d.unsafe
                { originPoint = eyePoint
                , xDirection = xDirection
                , yDirection = yDirection
                , zDirection = zDirection
                }

        Nothing ->
            case Vector3d.direction zVector of
                Just zDirection ->
                    -- The view vector must be parallel to the up direction,
                    -- since it is non-zero and therefore otherwise would have
                    -- resulted in a valid orthonormalization; therefore, choose
                    -- an arbitrary 'up' direction that is perpendicular to the
                    -- view direction
                    Frame3d.with
                        { originPoint = eyePoint
                        , zDirection = zDirection
                        }

                Nothing ->
                    -- The view vector is zero (the eye point and focal point
                    -- are coincident), so construct an arbitrary frame with the
                    -- given up direction
                    let
                        ( zDirection, xDirection ) =
                            Direction3d.perpendicularBasis upDirection
                    in
                    Frame3d.unsafe
                        { originPoint = eyePoint
                        , xDirection = xDirection
                        , yDirection = upDirection
                        , zDirection = zDirection
                        }


{-| Create a perspective camera with the common camera properties plus vertical
field of view given in radians. (The horizontal field of view will be chosen to
match the aspect ratio of the screen given by `screenWidth` and `screenHeight`.)

    perspectiveCamera =
        Camera.perspective
            { frame = cameraFrame
            , screenWidth = 1024
            , screenHeight = 768
            , verticalFieldOfView = degrees 30
            , nearClipDistance = 0.1
            , farClipDistance = 1000
            }

-}
perspective : { frame : Frame3d, screenWidth : Float, screenHeight : Float, verticalFieldOfView : Float, nearClipDistance : Float, farClipDistance : Float } -> Camera
perspective { frame, screenWidth, screenHeight, verticalFieldOfView, nearClipDistance, farClipDistance } =
    let
        aspectRatio =
            screenWidth / screenHeight

        fovInDegrees =
            verticalFieldOfView / degrees 1

        projectionMatrix =
            Matrix4.makePerspective fovInDegrees aspectRatio nearClipDistance farClipDistance
    in
    Camera
        { frame = frame
        , screenWidth = screenWidth
        , screenHeight = screenHeight
        , projectionMatrix = projectionMatrix
        }


{-| Create an orthographic camera with the common camera properties plus the
height of the orthographic viewport: this is the height in 3D model units of the
section of the model to be rendered. (The width will be chosen to match the
aspect ratio of the screen given by `screenWidth` and `screenHeight`.)

    orthographicCamera =
        Camera.orthographic
            { frame = cameraFrame
            , screenWidth = 1024
            , screenHeight = 768
            , viewportHeight = 5
            , nearClipDistance = 0.1
            , farClipDistance = 1000
            }

-}
orthographic : { frame : Frame3d, screenWidth : Float, screenHeight : Float, viewportHeight : Float, nearClipDistance : Float, farClipDistance : Float } -> Camera
orthographic { frame, screenWidth, screenHeight, viewportHeight, nearClipDistance, farClipDistance } =
    let
        aspectRatio =
            screenWidth / screenHeight

        viewportWidth =
            aspectRatio * viewportHeight

        left =
            -viewportWidth / 2

        right =
            viewportWidth / 2

        bottom =
            -viewportHeight / 2

        top =
            viewportHeight / 2

        projectionMatrix =
            Matrix4.makeOrtho left right bottom top nearClipDistance farClipDistance
    in
    Camera
        { frame = frame
        , screenWidth = screenWidth
        , screenHeight = screenHeight
        , projectionMatrix = projectionMatrix
        }


{-| Get the [view matrix](http://www.opengl-tutorial.org/beginners-tutorials/tutorial-3-matrices/#the-view-matrix)
of a camera. Multiplying by this matrix transforms from world coordinates to eye
coordinates.
-}
viewMatrix : Camera -> Mat4
viewMatrix camera =
    Frame3d.toMat4 (Frame3d.relativeTo (frame camera) Frame3d.xyz)


{-| Get the [projection matrix](http://www.opengl-tutorial.org/beginners-tutorials/tutorial-3-matrices/#the-projection-matrix)
of a camera. Multiplying by this matrix converts from eye coordinates to WebGL
normalized device coordinates.
-}
projectionMatrix : Camera -> Mat4
projectionMatrix (Camera properties) =
    properties.projectionMatrix


{-| Construct a WebGL model-view matrix given a camera and a `Frame3d` that
defines the position and orientation of an object;

    Camera.modelViewMatrix camera modelFrame

is equivalent to

    Matrix4.mul (Camera.viewMatrix camera) (Frame3d.toMat4 modelFrame)

Multiplying by this matrix transforms from object coordinates to eye
coordinates.

-}
modelViewMatrix : Camera -> Frame3d -> Mat4
modelViewMatrix camera modelFrame =
    Frame3d.toMat4 (Frame3d.relativeTo (frame camera) modelFrame)


{-| Get the full model-view-projection matrix given a camera and a `Frame3d`
that defines the position and orientation of an object;

    Camera.modelViewProjectionMatrix camera modelFrame

is equivalent to

    Matrix4.mul (Camera.projectionMatrix camera)
        (Camera.modelViewMatrix camera modelFrame)

-}
modelViewProjectionMatrix : Camera -> Frame3d -> Mat4
modelViewProjectionMatrix camera modelFrame =
    Matrix4.mul (projectionMatrix camera) (modelViewMatrix camera modelFrame)


{-| Get the frame defining the position and orientation of a camera.
-}
frame : Camera -> Frame3d
frame (Camera properties) =
    properties.frame


{-| Get the width of the screen rendered to by a camera.
-}
screenWidth : Camera -> Float
screenWidth (Camera properties) =
    properties.screenWidth


{-| Get the height of the screen rendered to by a camera.
-}
screenHeight : Camera -> Float
screenHeight (Camera properties) =
    properties.screenHeight
