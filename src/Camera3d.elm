module Camera3d exposing
    ( Camera3d
    , perspective, orthographic
    , viewpoint, screenWidth, screenHeight
    , viewMatrix, modelViewMatrix, projectionMatrix, modelViewProjectionMatrix
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

@docs viewpoint, screenWidth, screenHeight


# Matrices

@docs viewMatrix, modelViewMatrix, projectionMatrix, modelViewProjectionMatrix

-}

import Camera3d.Types as Types
import Frame3d exposing (Frame3d)
import Math.Matrix4 exposing (Mat4)
import Viewpoint3d exposing (Viewpoint3d)


{-| -}
type alias Camera3d =
    Types.Camera3d


makeViewProjectionRecord : Viewpoint3d -> Mat4 -> Types.Mat4Record
makeViewProjectionRecord givenViewpoint givenProjectionMatrix =
    let
        viewProjectionMatrix =
            Math.Matrix4.mul givenProjectionMatrix
                (Viewpoint3d.viewMatrix givenViewpoint)
    in
    Math.Matrix4.toRecord viewProjectionMatrix


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
perspective : { viewpoint : Viewpoint3d, screenWidth : Float, screenHeight : Float, verticalFieldOfView : Float, nearClipDistance : Float, farClipDistance : Float } -> Camera3d
perspective arguments =
    let
        aspectRatio =
            arguments.screenWidth / arguments.screenHeight

        fovInDegrees =
            arguments.verticalFieldOfView / degrees 1

        perspectiveProjectionMatrix =
            Math.Matrix4.makePerspective
                fovInDegrees
                aspectRatio
                arguments.nearClipDistance
                arguments.farClipDistance
    in
    Types.Camera3d
        { viewpoint = arguments.viewpoint
        , screenWidth = arguments.screenWidth
        , screenHeight = arguments.screenHeight
        , projectionMatrix = perspectiveProjectionMatrix
        , viewProjectionRecord =
            makeViewProjectionRecord
                arguments.viewpoint
                perspectiveProjectionMatrix
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
orthographic : { viewpoint : Viewpoint3d, screenWidth : Float, screenHeight : Float, viewportHeight : Float, nearClipDistance : Float, farClipDistance : Float } -> Camera3d
orthographic arguments =
    let
        aspectRatio =
            arguments.screenWidth / arguments.screenHeight

        viewportWidth =
            aspectRatio * arguments.viewportHeight

        left =
            -viewportWidth / 2

        right =
            viewportWidth / 2

        bottom =
            -arguments.viewportHeight / 2

        top =
            arguments.viewportHeight / 2

        orthographicProjectionMatrix =
            Math.Matrix4.makeOrtho
                left
                right
                bottom
                top
                arguments.nearClipDistance
                arguments.farClipDistance
    in
    Types.Camera3d
        { viewpoint = arguments.viewpoint
        , screenWidth = arguments.screenWidth
        , screenHeight = arguments.screenHeight
        , projectionMatrix = orthographicProjectionMatrix
        , viewProjectionRecord =
            makeViewProjectionRecord
                arguments.viewpoint
                orthographicProjectionMatrix
        }


{-| Get the viewpoint defining the position and orientation of a camera.
-}
viewpoint : Camera3d -> Viewpoint3d
viewpoint (Types.Camera3d properties) =
    properties.viewpoint


{-| Get the width of the screen rendered to by a camera.
-}
screenWidth : Camera3d -> Float
screenWidth (Types.Camera3d properties) =
    properties.screenWidth


{-| Get the height of the screen rendered to by a camera.
-}
screenHeight : Camera3d -> Float
screenHeight (Types.Camera3d properties) =
    properties.screenHeight


{-| Get the [view matrix](http://www.opengl-tutorial.org/beginners-tutorials/tutorial-3-matrices/#the-view-matrix)
of a camera;

    Camera3d.viewMatrix camera

is shorthand for

    Viewpoint3d.viewMatrix (Camera3d.viewpoint camera)

-}
viewMatrix : Camera3d -> Mat4
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
modelViewMatrix : Frame3d -> Camera3d -> Mat4
modelViewMatrix modelFrame camera =
    Viewpoint3d.modelViewMatrix modelFrame (viewpoint camera)


{-| Get the [projection matrix](http://www.opengl-tutorial.org/beginners-tutorials/tutorial-3-matrices/#the-projection-matrix)
of a camera. Multiplying by this matrix converts from eye coordinates to WebGL
normalized device coordinates.
-}
projectionMatrix : Camera3d -> Mat4
projectionMatrix (Types.Camera3d properties) =
    properties.projectionMatrix


{-| Get the full model-view-projection matrix given a camera and a `Frame3d`
that defines the position and orientation of an object;

    Camera3d.modelViewProjectionMatrix modelFrame camera

is equivalent to

    Matrix4.mul (Camera3d.projectionMatrix camera)
        (Camera3d.modelViewMatrix modelFrame camera)

-}
modelViewProjectionMatrix : Frame3d -> Camera3d -> Mat4
modelViewProjectionMatrix modelFrame camera =
    Math.Matrix4.mul
        (projectionMatrix camera)
        (modelViewMatrix modelFrame camera)
