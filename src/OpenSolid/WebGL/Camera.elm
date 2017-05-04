module OpenSolid.WebGL.Camera
    exposing
        ( Camera
        , perspective
        , orthographic
        , frame
        , screenWidth
        , screenHeight
        , projectionMatrix
        )

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.WebGL.Frame3d as Frame3d
import Math.Matrix4 as Matrix4 exposing (Mat4)


type Camera
    = Camera
        { frame : Frame3d
        , screenWidth : Float
        , screenHeight : Float
        , projectionMatrix : Mat4
        }


perspective : { frame : Frame3d, screenWidth : Float, screenHeight : Float, verticalFov : Float, zNear : Float, zFar : Float } -> Camera
perspective { frame, screenWidth, screenHeight, verticalFov, zNear, zFar } =
    let
        aspectRatio =
            screenWidth / screenHeight

        fovInDegrees =
            verticalFov / degrees 1

        projectionMatrix =
            Matrix4.makePerspective fovInDegrees aspectRatio zNear zFar
    in
        Camera
            { frame = frame
            , screenWidth = screenWidth
            , screenHeight = screenHeight
            , projectionMatrix = projectionMatrix
            }


orthographic : { frame : Frame3d, screenWidth : Float, screenHeight : Float, viewportHeight : Float, zNear : Float, zFar : Float } -> Camera
orthographic { frame, screenWidth, screenHeight, viewportHeight, zNear, zFar } =
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
        Camera
            { frame = frame
            , screenWidth = screenWidth
            , screenHeight = screenHeight
            , projectionMatrix = projectionMatrix
            }


frame : Camera -> Frame3d
frame (Camera properties) =
    properties.frame


screenWidth : Camera -> Float
screenWidth (Camera properties) =
    properties.screenWidth


screenHeight : Camera -> Float
screenHeight (Camera properties) =
    properties.screenHeight


projectionMatrix : Camera -> Mat4
projectionMatrix (Camera properties) =
    properties.projectionMatrix
