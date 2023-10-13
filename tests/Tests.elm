module Tests exposing (suite)

import Camera3d exposing (Camera3d)
import Camera3d.Random as Random exposing (ScreenCoordinates, WorldCoordinates)
import Expect
import Geometry.Expect as Expect
import Geometry.Interop.LinearAlgebra.Point3d as Point3d
import Geometry.Random as Random
import Length exposing (Meters)
import Pixels exposing (Pixels)
import Plane3d
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Point3d.Projection as Point3d
import Quantity exposing (Quantity, Unitless)
import Random exposing (Generator)
import Rectangle2d exposing (Rectangle2d)
import Test exposing (Test)
import Test.Random as Test
import WebGL.Matrices as WebGL


type NdcCoordinates
    = NdcCoordinates


ndcToScreen :
    Rectangle2d Pixels ScreenCoordinates
    -> Point3d Unitless NdcCoordinates
    -> Point2d Pixels ScreenCoordinates
ndcToScreen screenRectangle ndcPoint =
    let
        ( ndcX, ndcY, _ ) =
            Point3d.toTuple Quantity.toFloat ndcPoint
    in
    Rectangle2d.interpolate screenRectangle
        ((ndcX + 1) / 2)
        ((ndcY + 1) / 2)


validPoint : Camera3d Meters WorldCoordinates -> Point3d Meters WorldCoordinates -> Bool
validPoint camera point =
    let
        eyePoint =
            Camera3d.eyePoint camera

        viewDirection =
            Camera3d.viewDirection camera

        viewPlane =
            Plane3d.through eyePoint viewDirection
    in
    Point3d.signedDistanceFrom viewPlane point |> Quantity.greaterThan (Length.meters 0.1)


projectionParameters :
    Rectangle2d Pixels coordinates
    ->
        { nearClipDepth : Quantity Float Meters
        , farClipDepth : Quantity Float Meters
        , aspectRatio : Float
        }
projectionParameters screen =
    let
        ( width, height ) =
            Rectangle2d.dimensions screen
    in
    { nearClipDepth = Length.meters 0.1
    , farClipDepth = Length.meters 100
    , aspectRatio = Quantity.ratio width height
    }


genericTestCase : Generator { camera : Camera3d Meters WorldCoordinates, point : Point3d Meters pointCoordinates, screen : Rectangle2d Pixels ScreenCoordinates }
genericTestCase =
    Random.map3
        (\camera point screen ->
            { camera = camera
            , point = point
            , screen = screen
            }
        )
        Random.camera
        Random.point3d
        Random.screen


rayTestCase :
    Generator
        { camera : Camera3d Meters WorldCoordinates
        , point2d : Point2d Pixels ScreenCoordinates
        , screen : Rectangle2d Pixels ScreenCoordinates
        , distance : Quantity Float Meters
        }
rayTestCase =
    Random.map4
        (\camera screen ( u, v ) distance ->
            { camera = camera
            , point2d = Rectangle2d.interpolate screen u v
            , screen = screen
            , distance = distance
            }
        )
        Random.camera
        Random.screen
        (Random.map2 Tuple.pair Random.parameterValue Random.parameterValue)
        (Random.map Length.meters (Random.float 1 10))


suite : Test
suite =
    Test.describe "elm-3d-camera"
        [ Test.check "viewMatrix/projectionMatrix and toScreenSpace are consistent"
            genericTestCase
            (\{ camera, point, screen } ->
                if validPoint camera point then
                    let
                        firstPoint =
                            Point3d.toScreenSpace camera screen point

                        viewMatrix =
                            WebGL.viewMatrix camera

                        projectionMatrix =
                            WebGL.projectionMatrix camera
                                (projectionParameters screen)

                        secondPoint =
                            point
                                |> Point3d.transformBy viewMatrix
                                |> Point3d.transformBy projectionMatrix
                                |> ndcToScreen screen
                    in
                    firstPoint |> Expect.point2d secondPoint

                else
                    Expect.pass
            )
        , Test.check "viewProjectionMatrix and toScreenSpace are consistent"
            genericTestCase
            (\{ camera, point, screen } ->
                if validPoint camera point then
                    let
                        firstPoint =
                            Point3d.toScreenSpace camera screen point

                        viewProjectionMatrix =
                            WebGL.viewProjectionMatrix camera
                                (projectionParameters screen)

                        secondPoint =
                            point
                                |> Point3d.transformBy viewProjectionMatrix
                                |> ndcToScreen screen
                    in
                    firstPoint |> Expect.point2d secondPoint

                else
                    Expect.pass
            )
        , Test.check2 "modelViewMatrix/projectionMatrix and placeIn/toScreenSpace are consistent"
            genericTestCase
            Random.frame3d
            (\{ camera, point, screen } modelFrame ->
                let
                    globalPoint =
                        Point3d.placeIn modelFrame point
                in
                if validPoint camera globalPoint then
                    let
                        firstPoint =
                            Point3d.toScreenSpace camera screen globalPoint

                        modelViewMatrix =
                            WebGL.modelViewMatrix modelFrame camera

                        projectionMatrix =
                            WebGL.projectionMatrix camera
                                (projectionParameters screen)

                        secondPoint =
                            point
                                |> Point3d.transformBy modelViewMatrix
                                |> Point3d.transformBy projectionMatrix
                                |> ndcToScreen screen
                    in
                    firstPoint |> Expect.point2d secondPoint

                else
                    Expect.pass
            )
        , Test.check2 "modelViewMatrix/projectionMatrix and modelViewProjectionMatrix are consistent"
            genericTestCase
            Random.frame3d
            (\{ camera, point, screen } modelFrame ->
                let
                    globalPoint =
                        Point3d.placeIn modelFrame point
                in
                if validPoint camera globalPoint then
                    let
                        modelViewMatrix =
                            WebGL.modelViewMatrix modelFrame camera

                        projectionMatrix =
                            WebGL.projectionMatrix camera
                                (projectionParameters screen)

                        modelViewProjectionMatrix =
                            WebGL.modelViewProjectionMatrix
                                modelFrame
                                camera
                                (projectionParameters screen)

                        firstPoint =
                            point
                                |> Point3d.transformBy modelViewMatrix
                                |> Point3d.transformBy projectionMatrix
                                |> ndcToScreen screen

                        secondPoint =
                            point
                                |> Point3d.transformBy modelViewProjectionMatrix
                                |> ndcToScreen screen
                    in
                    firstPoint |> Expect.point2d secondPoint

                else
                    Expect.pass
            )
        , Test.check "ray and toScreenSpace are consistent"
            rayTestCase
            (\{ camera, screen, point2d, distance } ->
                let
                    ray =
                        Camera3d.ray camera screen point2d

                    point3d =
                        Point3d.along ray distance

                    projectedPoint =
                        Point3d.toScreenSpace camera screen point3d
                in
                projectedPoint |> Expect.point2dWithin (Pixels.pixels 1.0e-10) point2d
            )
        ]
