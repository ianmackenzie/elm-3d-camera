module OpenSolid.WebGL.Frame3d
    exposing
        ( lookAt
        , modelMatrix
        , modelViewMatrix
        , viewMatrix
        )

{-| Functions for constructing WebGL model and view matrices from `Frame3d`
values, and for constructing frames useful for defining camera positions and
orientations.

Note that according to OpenGL convention, the view direction of the camera is
the _negative_ Z direction of its frame. The positive Z direction of the frame
is 'out of the screen', the positive X direction is to the right and the
positive Y direction is up.

@docs modelMatrix, viewMatrix, modelViewMatrix, lookAt

-}

import Math.Matrix4 exposing (Mat4)
import OpenSolid.Direction3d as Direction3d exposing (Direction3d)
import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
import OpenSolid.Interop.LinearAlgebra.Frame3d as Frame3d
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)


{-| Construct a WebGL [model matrix](http://www.opengl-tutorial.org/beginners-tutorials/tutorial-3-matrices/#the-model-matrix)
from a `Frame3d` that defines the position and orientation of an object.
Multiplying by this matrix transforms from local (object) coordinates to global
(world) coordinates (just like calling one of the various OpenSolid `placeIn`
functions).
-}
modelMatrix : Frame3d -> Mat4
modelMatrix frame =
    Frame3d.toMat4 frame


{-| Construct a WebGL [view matrix](http://www.opengl-tutorial.org/beginners-tutorials/tutorial-3-matrices/#the-view-matrix)
from a `Frame3d` that defines the position and orientation of a camera.
Multiplying by this matrix transforms from world coordinates to eye coordinates.

Note that you will typically also need a [projection matrix](http://www.opengl-tutorial.org/beginners-tutorials/tutorial-3-matrices/#the-projection-matrix);
the `linear-algebra` library provides [several](http://package.elm-lang.org/packages/elm-community/linear-algebra/latest/Math-Matrix4#projections).

-}
viewMatrix : Frame3d -> Mat4
viewMatrix frame =
    Frame3d.toMat4 (Frame3d.relativeTo frame Frame3d.xyz)


{-| Construct a WebGL model-view matrix from one `Frame3d` that defines the
position and orientation of a camera and another that defines the position and
orientation of an object.

    Frame3d.modelViewMatrix eyeFrame modelFrame

is equivalent to

    Matrix4.mul (Frame3d.viewMatrix eyeFrame) (Frame3d.modelMatrix modelFrame)

-}
modelViewMatrix : Frame3d -> Frame3d -> Mat4
modelViewMatrix eyeFrame modelFrame =
    Frame3d.toMat4 (Frame3d.relativeTo eyeFrame modelFrame)


{-| Construct a `Frame3d` that can be used as the camera frame for a camera at
the given eye point looking towards the given focal point, with the given
global up direction (which will typically be `Direction3d.positiveZ` or
`Direction3d.positiveY`). That is likely all you need to know but if you are
interested in the details, read on!

The Z direction of the returned frame will point _from_ the focal point _to_ the
eye point (since as mentioned above OpenGL expects the viewing direction to be
the _negative_ Z direction). The Y direction will be chosen to be as close to
the global up direction as possible (the camera will not 'roll') and the X
direction will point to the right.

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
            Frame3d.with
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
                    let
                        ( xDirection, yDirection ) =
                            Direction3d.perpendicularBasis zDirection
                    in
                    Frame3d.with
                        { originPoint = eyePoint
                        , xDirection = xDirection
                        , yDirection = yDirection
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
                    Frame3d.with
                        { originPoint = eyePoint
                        , xDirection = xDirection
                        , yDirection = upDirection
                        , zDirection = zDirection
                        }
