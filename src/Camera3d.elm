module Camera3d
    exposing
        ( Camera3d
        , lineSegment2d
        , modelViewMatrix
        , modelViewProjectionMatrix
        , orthographic
        , perspective
        , point2d
        , polyline2d
        , projectionMatrix
        , screenHeight
        , screenWidth
        , triangle2d
        , viewMatrix
        , viewpoint
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


# Projection

@docs point2d, lineSegment2d, triangle2d, polyline2d

-}

import Basics.Extra exposing (inDegrees)
import Frame3d exposing (Frame3d)
import LineSegment2d exposing (LineSegment2d)
import LineSegment3d exposing (LineSegment3d)
import Math.Matrix4 exposing (Mat4)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Polyline2d exposing (Polyline2d)
import Polyline3d exposing (Polyline3d)
import Triangle2d exposing (Triangle2d)
import Triangle3d exposing (Triangle3d)
import Viewpoint3d exposing (Viewpoint3d)


type alias Mat4Record =
    { m11 : Float
    , m12 : Float
    , m13 : Float
    , m14 : Float
    , m21 : Float
    , m22 : Float
    , m23 : Float
    , m24 : Float
    , m31 : Float
    , m32 : Float
    , m33 : Float
    , m34 : Float
    , m41 : Float
    , m42 : Float
    , m43 : Float
    , m44 : Float
    }


{-| -}
type Camera3d
    = Camera3d
        { viewpoint : Viewpoint3d
        , projectionMatrix : Mat4
        , screenWidth : Float
        , screenHeight : Float
        , viewProjectionRecord : Mat4Record
        }


makeViewProjectionRecord : Viewpoint3d -> Mat4 -> Mat4Record
makeViewProjectionRecord viewpoint projectionMatrix =
    let
        viewMatrix =
            Viewpoint3d.viewMatrix viewpoint

        viewProjectionMatrix =
            Math.Matrix4.mul projectionMatrix viewMatrix
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
perspective { viewpoint, screenWidth, screenHeight, verticalFieldOfView, nearClipDistance, farClipDistance } =
    let
        aspectRatio =
            screenWidth / screenHeight

        fovInDegrees =
            verticalFieldOfView |> inDegrees

        projectionMatrix =
            Math.Matrix4.makePerspective
                fovInDegrees
                aspectRatio
                nearClipDistance
                farClipDistance
    in
    Camera3d
        { viewpoint = viewpoint
        , screenWidth = screenWidth
        , screenHeight = screenHeight
        , projectionMatrix = projectionMatrix
        , viewProjectionRecord =
            makeViewProjectionRecord viewpoint projectionMatrix
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
orthographic { viewpoint, screenWidth, screenHeight, viewportHeight, nearClipDistance, farClipDistance } =
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
            Math.Matrix4.makeOrtho
                left
                right
                bottom
                top
                nearClipDistance
                farClipDistance
    in
    Camera3d
        { viewpoint = viewpoint
        , screenWidth = screenWidth
        , screenHeight = screenHeight
        , projectionMatrix = projectionMatrix
        , viewProjectionRecord =
            makeViewProjectionRecord viewpoint projectionMatrix
        }


{-| Get the viewpoint defining the position and orientation of a camera.
-}
viewpoint : Camera3d -> Viewpoint3d
viewpoint (Camera3d properties) =
    properties.viewpoint


{-| Get the width of the screen rendered to by a camera.
-}
screenWidth : Camera3d -> Float
screenWidth (Camera3d properties) =
    properties.screenWidth


{-| Get the height of the screen rendered to by a camera.
-}
screenHeight : Camera3d -> Float
screenHeight (Camera3d properties) =
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
projectionMatrix (Camera3d properties) =
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


viewProjectionRecord : Camera3d -> Mat4Record
viewProjectionRecord (Camera3d properties) =
    properties.viewProjectionRecord


{-| Convert a point from 3D world space to 2D screen (pixel) coordinates. The
result will be in a coordinate system where (0,0) is the bottom left of the
screen.
-}
point2d : Camera3d -> Point3d -> Point2d
point2d (Camera3d camera) point =
    let
        { m11, m12, m13, m14, m21, m22, m23, m24, m41, m42, m43, m44 } =
            camera.viewProjectionRecord

        halfWidth =
            0.5 * camera.screenWidth

        halfHeight =
            0.5 * camera.screenHeight

        ( x, y, z ) =
            Point3d.coordinates point

        w =
            m41 * x + m42 * y + m43 * z + m44

        ndcX =
            (m11 * x + m12 * y + m13 * z + m14) / w

        ndcY =
            (m21 * x + m22 * y + m23 * z + m24) / w
    in
    Point2d.fromCoordinates
        ( halfWidth + halfWidth * ndcX
        , halfHeight + halfHeight * ndcY
        )


{-| Convert a line segment from 3D space to 2D screen coordinates.
-}
lineSegment2d : Camera3d -> LineSegment3d -> LineSegment2d
lineSegment2d camera lineSegment =
    let
        ( p1, p2 ) =
            LineSegment3d.endpoints lineSegment
    in
    LineSegment2d.fromEndpoints ( point2d camera p1, point2d camera p2 )


{-| Convert a triangle from 3D space to 2D screen coordinates.
-}
triangle2d : Camera3d -> Triangle3d -> Triangle2d
triangle2d camera triangle =
    let
        ( p1, p2, p3 ) =
            Triangle3d.vertices triangle
    in
    Triangle2d.fromVertices
        ( point2d camera p1
        , point2d camera p2
        , point2d camera p3
        )


{-| Convert a polyline from 3D space to 2D screen coordinates.
-}
polyline2d : Camera3d -> Polyline3d -> Polyline2d
polyline2d camera polyline =
    Polyline3d.vertices polyline
        |> List.map (point2d camera)
        |> Polyline2d.fromVertices
