module OpenSolid.WebGL.Camera
    exposing
        ( Camera
        , frame
        , orthographic
        , perspective
        , projectionMatrix
        , screenHeight
        , screenWidth
        )

import Math.Matrix4 as Matrix4 exposing (Mat4)
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.WebGL.Frame3d as Frame3d


type Camera
    = Camera
        { frame : Frame3d
        , screenWidth : Float
        , screenHeight : Float
        , projectionMatrix : Mat4
        }


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
