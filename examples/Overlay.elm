module Overlay exposing (Model, Msg(..), ProjectionType(..), main, update, view)

import Angle
import Axis3d
import BoundingBox2d
import Browser
import Camera3d
import Circle2d
import Color
import Direction3d
import Drawing2d
import Element
import Element.Border
import Element.Input
import Frame2d
import Html exposing (Html)
import Length exposing (meters)
import LineSegment3d
import LineSegment3d.Projection as LineSegment3d
import Logo exposing (logoUnits)
import Pixels exposing (inPixels, pixels)
import Point2d
import Point3d
import Point3d.Projection as Point3d
import Quantity exposing (zero)
import Rectangle2d
import Viewpoint3d


type ProjectionType
    = Perspective
    | Orthographic


type Msg
    = SetAngleInDegrees Float
    | SetProjectionType ProjectionType


type alias Model =
    { angleInDegrees : Float
    , projectionType : ProjectionType
    }


projectionTypeString : ProjectionType -> String
projectionTypeString projectionType =
    case projectionType of
        Perspective ->
            "Perspective"

        Orthographic ->
            "Orthographic"


update : Msg -> Model -> Model
update message model =
    case message of
        SetAngleInDegrees angleInDegrees ->
            { model | angleInDegrees = angleInDegrees }

        SetProjectionType projectionType ->
            { model | projectionType = projectionType }


view : Model -> Html Msg
view { angleInDegrees, projectionType } =
    let
        screenWidth =
            pixels 500

        screenHeight =
            pixels 400

        screenCenter =
            Frame2d.atPoint (Point2d.pixels 450 300)
                |> Frame2d.rotateBy (Angle.degrees 22.5)

        screen =
            Rectangle2d.centeredOn screenCenter ( screenWidth, screenHeight )

        eyePoint =
            Point3d.xyz (logoUnits 4) zero zero
                |> Point3d.rotateAround Axis3d.y (Angle.degrees -22.5)
                |> Point3d.rotateAround Axis3d.z (Angle.degrees 60)

        viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = Point3d.origin
                , eyePoint = eyePoint
                , upDirection = Direction3d.z
                }

        camera =
            case projectionType of
                Perspective ->
                    Camera3d.perspective
                        { viewpoint = viewpoint
                        , verticalFieldOfView = Angle.degrees 30
                        }

                Orthographic ->
                    Camera3d.orthographic
                        { viewpoint = viewpoint
                        , viewportHeight = logoUnits 2
                        }

        angle =
            Angle.degrees angleInDegrees

        vertices2d =
            Logo.vertices
                |> List.map (Point3d.rotateAround Axis3d.z angle)
                |> List.map (Point3d.toScreenSpace camera screen)

        drawingCircles =
            vertices2d
                |> List.map
                    (\vertex ->
                        Drawing2d.circle
                            [ Drawing2d.strokeColor Color.grey
                            , Drawing2d.strokeWidth (pixels 1)
                            , Drawing2d.noFill
                            ]
                            (Circle2d.withRadius (pixels 3) vertex)
                    )

        drawingLines =
            Logo.edges
                |> List.map (LineSegment3d.rotateAround Axis3d.z angle)
                |> List.map (LineSegment3d.toScreenSpace camera screen)
                |> List.map
                    (\edge ->
                        Drawing2d.lineSegment
                            [ Drawing2d.strokeColor Color.grey
                            , Drawing2d.strokeWidth (pixels 1)

                            --, Attributes.strokeDasharray "5 5"
                            ]
                            edge
                    )

        drawingBounds =
            BoundingBox2d.fromExtrema
                { minX = zero
                , minY = zero
                , maxX = pixels 800
                , maxY = pixels 600
                }

        drawingElement =
            Drawing2d.toHtml
                { viewBox = drawingBounds
                , size = Drawing2d.fixed 
                }
                []
                (Drawing2d.rectangle [ Drawing2d.noFill ] screen
                    :: (drawingLines ++ drawingCircles)
                )

        slider =
            Element.el [ Element.paddingXY 0 6 ] <|
                Element.Input.slider
                    [ Element.width (Element.px 400)
                    , Element.Border.width 1
                    , Element.height (Element.px 6)
                    , Element.Border.rounded 4
                    , Element.Border.color (Element.rgb 0.75 0.75 0.75)
                    ]
                    { onChange = SetAngleInDegrees
                    , label =
                        Element.Input.labelAbove []
                            (Element.text "Drag to rotate")
                    , min = 0
                    , max = 360
                    , value = angleInDegrees
                    , step = Just 1
                    , thumb = Element.Input.defaultThumb
                    }

        radioOption ownProjectionType =
            Element.Input.option ownProjectionType
                (Element.text (projectionTypeString ownProjectionType))

        radioButtons =
            Element.Input.radio []
                { onChange = SetProjectionType
                , selected = Just projectionType
                , label =
                    Element.Input.labelAbove []
                        (Element.text "Projection type")
                , options =
                    [ radioOption Perspective
                    , radioOption Orthographic
                    ]
                }
    in
    Element.layout [] <|
        Element.column [ Element.spacing 8 ]
            [ Element.el [ Element.Border.width 1 ]
                (Element.html drawingElement)
            , slider
            , radioButtons
            ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = { angleInDegrees = 0.0, projectionType = Perspective }
        , view = view
        , update = update
        }
