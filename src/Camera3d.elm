module Camera3d exposing
    ( Camera3d
    , perspective, orthographic
    , viewpoint
    , ray
    , viewMatrix, modelViewMatrix, projectionMatrix, viewProjectionMatrix, modelViewProjectionMatrix
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

@docs perspective, orthographic


# Properties

@docs viewpoint


# Ray casting

@docs ray


# WebGL rendering

These matrices can be used for rendering 3D [WebGL](https://package.elm-lang.org/packages/elm-explorations/webgl/latest/)
scenes. For in-depth explanations of how these matrices are used, check out

  - <https://learnopengl.com/Getting-started/Coordinate-Systems>
  - <http://www.opengl-tutorial.org/beginners-tutorials/tutorial-3-matrices/>
  - <http://www.songho.ca/opengl/gl_transform.html>

@docs viewMatrix, modelViewMatrix, projectionMatrix, viewProjectionMatrix, modelViewProjectionMatrix

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


{-| Create a perspective camera from a viewpoint and a vertical field of view.

    perspectiveCamera =
        Camera3d.perspective
            { viewpoint = cameraViewpoint
            , verticalFieldOfView = Angle.degrees 30
            }

-}
perspective :
    { viewpoint : Viewpoint3d units coordinates
    , verticalFieldOfView : Angle
    }
    -> Camera3d units coordinates
perspective arguments =
    let
        halfFieldOfView =
            Quantity.half (Quantity.abs arguments.verticalFieldOfView)

        frustumSlope =
            Angle.tan halfFieldOfView
    in
    Types.Camera3d
        { viewpoint = arguments.viewpoint
        , projection = Types.Perspective frustumSlope
        }


{-| Create an orthographic camera from a viewpoint, a clip depth and the height
of the orthographic viewport: this is the height in 3D world units of the
section of the model to be rendered.

    orthographicCamera =
        Camera3d.orthographic
            { viewpoint = cameraViewpoint
            , viewportHeight = Length.meters 5
            }

-}
orthographic :
    { viewpoint : Viewpoint3d units coordinates
    , viewportHeight : Quantity Float units
    }
    -> Camera3d units coordinates
orthographic arguments =
    Types.Camera3d
        { viewpoint = arguments.viewpoint
        , projection = Types.Orthographic (Quantity.abs arguments.viewportHeight)
        }


{-| Get the viewpoint defining the position and orientation of a camera.
-}
viewpoint : Camera3d units coordinates -> Viewpoint3d units coordinates
viewpoint (Types.Camera3d camera) =
    camera.viewpoint


{-| Given a camera, a rectangle defining the shape and size of a screen, and a
2D point within that screen, calculate the corresponding 3D ray as an `Axis3d`.
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


{-| Construct a WebGL view matrix for a given camera. Multiplying by this matrix
transforms from world coordinates to camera (eye) coordinates.
-}
viewMatrix : Camera3d units coordinates -> Mat4
viewMatrix camera =
    Viewpoint3d.viewMatrix (viewpoint camera)


{-| Construct a WebGL model-view matrix given a camera and a `Frame3d` that
defines the position and orientation of an object. Multiplying by this matrix
transforms from local object coordinates (coordinates relative to the given
frame) to camera (eye) coordinates.
-}
modelViewMatrix : Frame3d units coordinates defines -> Camera3d units coordinates -> Mat4
modelViewMatrix modelFrame camera =
    Viewpoint3d.modelViewMatrix modelFrame (viewpoint camera)


{-| Construct a WebGL projection matrix for a given camera, by supplying near
and far clip distances as well as the aspect ratio (width over height) of the
WebGL window being rendered to. Refer to the [above resources](#webgl-rendering)
for details of how projection matrices are defined and used.
-}
projectionMatrix :
    { nearClipDistance : Quantity Float units
    , farClipDistance : Quantity Float units
    , aspectRatio : Float
    }
    -> Camera3d units coordinates
    -> Mat4
projectionMatrix { nearClipDistance, farClipDistance, aspectRatio } (Types.Camera3d camera) =
    let
        (Quantity n) =
            Quantity.abs nearClipDistance

        (Quantity f) =
            Quantity.abs farClipDistance
    in
    case camera.projection of
        Types.Perspective frustumSlope ->
            Math.Matrix4.fromRecord
                { m11 = 1 / (aspectRatio * frustumSlope)
                , m21 = 0
                , m31 = 0
                , m41 = 0
                , m12 = 0
                , m22 = 1 / frustumSlope
                , m32 = 0
                , m42 = 0
                , m13 = 0
                , m23 = 0
                , m33 = -(f + n) / (f - n)
                , m43 = -1
                , m14 = 0
                , m24 = 0
                , m34 = -2 * f * n / (f - n)
                , m44 = 0
                }

        Types.Orthographic (Quantity viewportHeight) ->
            Math.Matrix4.fromRecord
                { m11 = 2 / (aspectRatio * viewportHeight)
                , m21 = 0
                , m31 = 0
                , m41 = 0
                , m12 = 0
                , m22 = 2 / viewportHeight
                , m32 = 0
                , m42 = 0
                , m13 = 0
                , m23 = 0
                , m33 = -2 / (f - n)
                , m43 = 0
                , m14 = 0
                , m24 = 0
                , m34 = -(f + n) / (f - n)
                , m44 = 1
                }


{-| Construct a WebGL view-projection matrix for a given camera; this is the
product of the projection and view matrices.
-}
viewProjectionMatrix :
    { nearClipDistance : Quantity Float units
    , farClipDistance : Quantity Float units
    , aspectRatio : Float
    }
    -> Camera3d units coordinates
    -> Mat4
viewProjectionMatrix projectionParameters camera =
    Math.Matrix4.mul
        (projectionMatrix projectionParameters camera)
        (viewMatrix camera)


{-| Construct a WebGL model-view-projection matrix for a given camera; this is
the product of the projection, view and model matrices.
-}
modelViewProjectionMatrix :
    Frame3d units coordinates defines
    ->
        { nearClipDistance : Quantity Float units
        , farClipDistance : Quantity Float units
        , aspectRatio : Float
        }
    -> Camera3d units coordinates
    -> Mat4
modelViewProjectionMatrix modelFrame projectionParameters camera =
    Math.Matrix4.mul
        (projectionMatrix projectionParameters camera)
        (modelViewMatrix modelFrame camera)
