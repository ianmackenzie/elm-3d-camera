module OpenSolid.WebGL.Frame3d
    exposing
        ( lookAt
        , modelMatrix
        , modelViewMatrix
        , viewMatrix
        )

{-| Functions for constructing WebGL model and view matrices from `Frame3d`
values.

@docs modelMatrix, viewMatrix, modelViewMatrix, lookAt

-}

import Math.Matrix4 exposing (Mat4)
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Frame3d as Frame3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Interop.LinearAlgebra.Frame3d as Frame3d
import OpenSolid.Point3d as Point3d
import OpenSolid.Vector3d as Vector3d


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

Note that according to OpenGL convention, the view direction is the _negative_ Z
direction of the frame. The positive Z direction of the frame is 'out of the
screen', the positive X direction is to the right and the positive Y direction
is up.

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

but more accurate (since internally `Mat4` values use single-precision floats
instead of double-precision).

-}
modelViewMatrix : Frame3d -> Frame3d -> Mat4
modelViewMatrix eyeFrame modelFrame =
    Frame3d.toMat4 (Frame3d.relativeTo eyeFrame modelFrame)


lookAt : { focalPoint : Point3d, eyePoint : Point3d, upDirection : Direction3d } -> Frame3d
lookAt { focalPoint, eyePoint, upDirection } =
    let
        zVector =
            Point3d.vectorFrom focalPoint eyePoint

        yVector =
            Direction3d.toVector upDirection

        xVector =
            Vector3d.crossProduct yVector zVector
    in
    case Vector3d.orthonormalize ( zVector, yVector, xVector ) of
        Just ( zDirection, yDirection, xDirection ) ->
            Frame3d
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
                    Frame3d
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
                    Frame3d
                        { originPoint = eyePoint
                        , xDirection = xDirection
                        , yDirection = upDirection
                        , zDirection = zDirection
                        }
