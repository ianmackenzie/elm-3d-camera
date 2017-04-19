module OpenSolid.WebGL.Projection
    exposing
        ( Projection
        , perspective
        , orthographic
        , eyeFrame
        , screenWidth
        , screenHeight
        , matrix
        )

import OpenSolid.Geometry.Types exposing (..)
import Math.Matrix4 as Matrix4 exposing (Mat4)


type Projection
    = Projection
        { eyeFrame : Frame3d
        , screenWidth : Float
        , screenHeight : Float
        , matrix : Mat4
        }


perspective : { eyeFrame : Frame3d, screenWidth : Float, screenHeight : Float, verticalFov : Float, zNear : Float, zFar : Float } -> Projection
perspective { eyeFrame, screenWidth, screenHeight, verticalFov, zNear, zFar } =
    let
        aspectRatio =
            screenWidth / screenHeight

        fovInDegrees =
            verticalFov / degrees 1

        projectionMatrix =
            Matrix4.makePerspective fovInDegrees aspectRatio zNear zFar
    in
        Projection
            { eyeFrame = eyeFrame
            , screenWidth = screenWidth
            , screenHeight = screenHeight
            , matrix = projectionMatrix
            }


orthographic : { eyeFrame : Frame3d, screenWidth : Float, screenHeight : Float, viewportHeight : Float, zNear : Float, zFar : Float } -> Projection
orthographic { eyeFrame, screenWidth, screenHeight, viewportHeight, zNear, zFar } =
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
            Matrix4.makeOrtho left right bottom top zNear zFar
    in
        Projection
            { eyeFrame = eyeFrame
            , screenWidth = screenWidth
            , screenHeight = screenHeight
            , matrix = projectionMatrix
            }


eyeFrame : Projection -> Frame3d
eyeFrame (Projection properties) =
    properties.eyeFrame


screenWidth : Projection -> Float
screenWidth (Projection properties) =
    properties.screenWidth


screenHeight : Projection -> Float
screenHeight (Projection properties) =
    properties.screenHeight


matrix : Projection -> Mat4
matrix (Projection properties) =
    properties.matrix
