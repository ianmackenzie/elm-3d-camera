module OpenSolid.WebGL.Projection
    exposing
        ( Projection
        , perspective
        , orthographic
        , matrix
        )

import Math.Matrix4 as Matrix4 exposing (Mat4)


type Projection
    = Projection Mat4


perspective : { verticalFov : Float, aspectRatio : Float, zNear : Float, zFar : Float } -> Projection
perspective { verticalFov, aspectRatio, zNear, zFar } =
    let
        fovInDegrees =
            verticalFov / degrees 1
    in
        Projection (Matrix4.makePerspective fovInDegrees aspectRatio zNear zFar)


orthographic : { height : Float, aspectRatio : Float, zNear : Float, zFar : Float } -> Projection
orthographic { height, aspectRatio, zNear, zFar } =
    let
        width =
            aspectRatio * height

        left =
            -width / 2

        right =
            width / 2

        bottom =
            -height / 2

        top =
            height / 2
    in
        Projection (Matrix4.makeOrtho left right bottom top zNear zFar)


matrix : Projection -> Mat4
matrix (Projection matrix_) =
    matrix_
