module OpenSolid.WebGL.Frame3d
    exposing
        ( modelMatrix
        , viewMatrix
        )

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.WebGL.Point3d as Point3d
import OpenSolid.WebGL.Direction3d as Direction3d
import Math.Matrix4 exposing (Mat4)


modelMatrix : Frame3d -> Mat4
modelMatrix frame =
    let
        (Frame3d { originPoint, xDirection, yDirection, zDirection }) =
            frame

        translationMatrix =
            Math.Matrix4.makeTranslate
                (Point3d.toVec3 originPoint)

        rotationMatrix =
            Math.Matrix4.makeBasis
                (Direction3d.toVec3 xDirection)
                (Direction3d.toVec3 yDirection)
                (Direction3d.toVec3 zDirection)
    in
        Math.Matrix4.mul translationMatrix rotationMatrix


viewMatrix : Frame3d -> Mat4
viewMatrix =
    modelMatrix >> Math.Matrix4.inverseOrthonormal
