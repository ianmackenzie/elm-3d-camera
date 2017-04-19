module OpenSolid.WebGL.Projection
    exposing
        ( Projection
        , perspective
        , orthographic
        , matrix
        )

import Math.Matrix4 as Matrix4 exposing (Mat4)


type Projection
    = Projection Float Float Mat4


perspective : { screenWidth : Float, screenHeight : Float, verticalFov : Float, zNear : Float, zFar : Float } -> Projection
perspective { screenWidth, screenHeight, verticalFov, zNear, zFar } =
    let
        aspectRatio =
            screenWidth / screenHeight

        fovInDegrees =
            verticalFov / degrees 1

        projectionMatrix =
            Matrix4.makePerspective fovInDegrees aspectRatio zNear zFar
    in
        Projection screenWidth screenHeight projectionMatrix


orthographic : { screenWidth : Float, screenHeight : Float, viewportHeight : Float, zNear : Float, zFar : Float } -> Projection
orthographic { screenWidth, screenHeight, viewportHeight, zNear, zFar } =
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
        Projection screenWidth screenHeight projectionMatrix


matrix : Projection -> Mat4
matrix (Projection _ _ matrix_) =
    matrix_
