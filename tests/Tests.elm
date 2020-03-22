module Tests exposing (suite)

import Camera3d exposing (Camera3d)
import Camera3d.Fuzz as Fuzz exposing (ScreenCoordinates, WorldCoordinates)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Geometry.Interop.LinearAlgebra.Point3d as Point3d
import Length exposing (Meters)
import Pixels exposing (Pixels)
import Plane3d
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Point3d.Projection as Point3d
import Quantity exposing (Quantity, Unitless)
import Rectangle2d exposing (Rectangle2d)
import SketchPlane3d
import Test exposing (Test)
import Viewpoint3d
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
        viewpoint =
            Camera3d.viewpoint camera

        eyePoint =
            Viewpoint3d.eyePoint viewpoint

        viewDirection =
            Viewpoint3d.viewDirection viewpoint

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


genericTestCase : Fuzzer { camera : Camera3d Meters WorldCoordinates, point : Point3d Meters pointCoordinates, screen : Rectangle2d Pixels ScreenCoordinates }
genericTestCase =
    Fuzz.map3
        (\camera point screen ->
            { camera = camera
            , point = point
            , screen = screen
            }
        )
        Fuzz.camera
        Fuzz.point3d
        Fuzz.screen


rayTestCase :
    Fuzzer
        { camera : Camera3d Meters WorldCoordinates
        , point2d : Point2d Pixels ScreenCoordinates
        , screen : Rectangle2d Pixels ScreenCoordinates
        , distance : Quantity Float Meters
        }
rayTestCase =
    Fuzz.map4
        (\camera screen ( u, v ) distance ->
            { camera = camera
            , point2d = Rectangle2d.interpolate screen u v
            , screen = screen
            , distance = distance
            }
        )
        Fuzz.camera
        Fuzz.screen
        (Fuzz.tuple ( Fuzz.parameterValue, Fuzz.parameterValue ))
        (Fuzz.quantityRange (Length.meters 1) (Length.meters 10))


suite : Test
suite =
    Test.describe "elm-3d-camera"
        [ Test.fuzz genericTestCase
            "viewMatrix/projectionMatrix and toScreenSpace are consistent"
            (\{ camera, point, screen } ->
                if validPoint camera point then
                    let
                        firstPoint =
                            Point3d.toScreenSpace camera screen point

                        viewMatrix =
                            WebGL.viewMatrix (Camera3d.viewpoint camera)

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
        , Test.fuzz genericTestCase
            "viewProjectionMatrix and toScreenSpace are consistent"
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
        , Test.fuzz2
            genericTestCase
            Fuzz.frame3d
            "modelViewMatrix/projectionMatrix and placeIn/toScreenSpace are consistent"
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
                            WebGL.modelViewMatrix modelFrame
                                (Camera3d.viewpoint camera)

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
        , Test.fuzz2
            genericTestCase
            Fuzz.frame3d
            "modelViewMatrix/projectionMatrix and modelViewProjectionMatrix are consistent"
            (\{ camera, point, screen } modelFrame ->
                let
                    globalPoint =
                        Point3d.placeIn modelFrame point
                in
                if validPoint camera globalPoint then
                    let
                        modelViewMatrix =
                            WebGL.modelViewMatrix modelFrame
                                (Camera3d.viewpoint camera)

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
        , Test.fuzz
            rayTestCase
            "ray and toScreenSpace are consistent"
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
