module Overlay exposing (..)

import Axis3d
import Camera3d
import Circle2d
import Direction3d
import Frame2d
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes as Attributes
import Kintail.InputWidget as InputWidget
import LineSegment3d
import LineSegment3d.Projection as LineSegment3d
import Logo
import Point2d
import Point3d
import Point3d.Projection as Point3d
import Svg exposing (Svg)
import Svg.Attributes
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
        width =
            800

        height =
            600

        eyePoint =
            Point3d.fromCoordinates ( 4, 0, 0 )
                |> Point3d.rotateAround Axis3d.y (degrees -22.5)
                |> Point3d.rotateAround Axis3d.z (degrees 60)

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
                        , screenWidth = toFloat width
                        , screenHeight = toFloat height
                        , verticalFieldOfView = degrees 30
                        , nearClipDistance = 0.1
                        , farClipDistance = 100
                        }

                Orthographic ->
                    Camera3d.orthographic
                        { viewpoint = viewpoint
                        , screenWidth = toFloat width
                        , screenHeight = toFloat height
                        , viewportHeight = 2
                        , nearClipDistance = 0.1
                        , farClipDistance = 100
                        }

        angle =
            degrees angleInDegrees

        vertices2d =
            Logo.vertices
                |> List.map (Point3d.rotateAround Axis3d.z angle)
                |> List.map (Point3d.toScreenSpace camera)

        svgCircles =
            vertices2d
                |> List.map
                    (\vertex ->
                        Svg.circle2d
                            [ Svg.Attributes.stroke "grey"
                            , Svg.Attributes.strokeWidth "2"
                            , Svg.Attributes.fill "none"
                            ]
                            (Circle2d.withRadius 7 vertex)
                    )

        svgLines =
            Logo.edges
                |> List.map (LineSegment3d.rotateAround Axis3d.z angle)
                |> List.map (LineSegment3d.toScreenSpace camera)
                |> List.map
                    (\edge ->
                        Svg.lineSegment2d
                            [ Svg.Attributes.stroke "grey"
                            , Svg.Attributes.strokeWidth "2"
                            , Svg.Attributes.strokeDasharray "5 5"
                            ]
                            edge
                    )

        topLeftFrame =
            Frame2d.atPoint (Point2d.fromCoordinates ( 0, height ))
                |> Frame2d.reverseY

        svgElement =
            Svg.svg
                [ Attributes.width width, Attributes.height height ]
                [ Svg.relativeTo topLeftFrame
                    (Svg.g [] (svgCircles ++ svgLines))
                ]

        sliderAttributes =
            [ Attributes.style [ ( "width", toString width ++ "px" ) ] ]

        sliderConfig =
            { min = 0
            , max = 360
            , step = 1
            }

        slider =
            Html.div []
                [ InputWidget.slider sliderAttributes
                    sliderConfig
                    angleInDegrees
                    |> Html.map SetAngleInDegrees
                ]

        radioButton ownProjectionType =
            let
                label =
                    toString ownProjectionType

                id =
                    String.toLower label
            in
            Html.div []
                [ InputWidget.radioButton [ Attributes.id id ]
                    ownProjectionType
                    projectionType
                    |> Html.map SetProjectionType
                , Html.label [ Attributes.for id ] [ Html.text label ]
                ]
    in
    Html.div []
        [ Html.div [] [ svgElement ]
        , slider
        , radioButton Perspective
        , radioButton Orthographic
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = { angleInDegrees = 0.0, projectionType = Perspective }
        , view = view
        , update = update
        }
