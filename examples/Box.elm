module Box exposing (..)

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Frame3d as Frame3d
import OpenSolid.Axis3d as Axis3d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Point2d as Point2d
import OpenSolid.Point3d as Point3d
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Triangle3d as Triangle3d
import OpenSolid.SketchPlane3d as SketchPlane3d
import OpenSolid.WebGL.Frame3d as Frame3d
import OpenSolid.WebGL.Vector3d as Vector3d
import OpenSolid.WebGL.Direction3d as Direction3d
import OpenSolid.WebGL.Point3d as Point3d
import OpenSolid.WebGL.Color as Color
import Touch exposing (Touch, TouchEvent(..))
import SingleTouch
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import Math.Matrix4 exposing (Mat4)
import WebGL exposing (Mesh)
import WebGL.Settings
import Mouse
import Task
import Color exposing (Color)
import Window
import Html exposing (Html, Attribute)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)


-- Types


type Msg
    = StartRotatingAt Point2d
    | PointerMovedTo Point2d
    | StopRotating
    | SetWindowSize Window.Size


type alias Model =
    { cubeFrame : Frame3d
    , dragPoint : Maybe Point2d
    , windowSize : Maybe Window.Size
    }


type alias Attributes =
    { vertexPosition : Vec3
    , vertexNormal : Vec3
    }


type alias Uniforms =
    { modelMatrix : Mat4
    , viewMatrix : Mat4
    , projectionMatrix : Mat4
    , lightDirection : Vec3
    , faceColor : Vec4
    }


type alias Varyings =
    { position : Vec3
    , normal : Vec3
    }



-- Constants


initialCubeFrame : Frame3d
initialCubeFrame =
    Frame3d.xyz
        |> Frame3d.rotateAround Axis3d.z (degrees -30)
        |> Frame3d.rotateAround Axis3d.y (degrees 20)


lightDirection : Direction3d
lightDirection =
    Vector3d ( -1, -1, -2 )
        |> Vector3d.direction
        |> Maybe.withDefault (Direction3d.flip Direction3d.z)


faceColor : Color
faceColor =
    Color.rgb 51 77 230


eyeFrame : Frame3d
eyeFrame =
    Frame3d
        { originPoint = Point3d ( 50, 0, 0 )
        , xDirection = Direction3d.y
        , yDirection = Direction3d.z
        , zDirection = Direction3d.x
        }



-- Rendering


positionsAndNormals : Triangle3d -> ( Attributes, Attributes, Attributes )
positionsAndNormals triangle =
    let
        ( p1, p2, p3 ) =
            Triangle3d.vertices triangle

        normalVector =
            Triangle3d.normalDirection triangle
                |> Maybe.map Direction3d.toVector
                |> Maybe.withDefault Vector3d.zero
                |> Vector3d.toVec3
    in
        ( { vertexPosition = Point3d.toVec3 p1, vertexNormal = normalVector }
        , { vertexPosition = Point3d.toVec3 p2, vertexNormal = normalVector }
        , { vertexPosition = Point3d.toVec3 p3, vertexNormal = normalVector }
        )


cubeMesh : Mesh Attributes
cubeMesh =
    let
        ( p0, p1, p2, p3, p4, p5, p6, p7 ) =
            ( Point3d ( -5, -5, -5 )
            , Point3d ( 5, -5, -5 )
            , Point3d ( 5, 5, -5 )
            , Point3d ( -5, 5, -5 )
            , Point3d ( -5, -5, 5 )
            , Point3d ( 5, -5, 5 )
            , Point3d ( 5, 5, 5 )
            , Point3d ( -5, 5, 5 )
            )

        faces =
            [ Triangle3d ( p0, p1, p5 )
            , Triangle3d ( p0, p5, p4 )
            , Triangle3d ( p1, p2, p6 )
            , Triangle3d ( p1, p6, p5 )
            , Triangle3d ( p4, p5, p6 )
            , Triangle3d ( p4, p6, p7 )
            , Triangle3d ( p3, p6, p2 )
            , Triangle3d ( p3, p7, p6 )
            , Triangle3d ( p3, p4, p7 )
            , Triangle3d ( p3, p0, p4 )
            , Triangle3d ( p0, p2, p1 )
            , Triangle3d ( p0, p3, p2 )
            ]
    in
        WebGL.triangles (List.map positionsAndNormals faces)


vertexShader : WebGL.Shader Attributes Uniforms Varyings
vertexShader =
    [glsl|
        attribute vec3 vertexPosition;
        attribute vec3 vertexNormal;
        uniform mat4 viewMatrix;
        uniform mat4 modelMatrix;
        uniform mat4 projectionMatrix;
        varying vec3 position;
        varying vec3 normal;

        void main () {
          gl_Position = projectionMatrix * viewMatrix * modelMatrix * vec4(vertexPosition, 1.0);
          position = (modelMatrix * vec4(vertexPosition, 1.0)).xyz;
          normal = (modelMatrix * vec4(vertexNormal, 0.0)).xyz;
        }
    |]


fragmentShader : WebGL.Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;
        uniform vec3 lightDirection;
        uniform vec4 faceColor;
        varying vec3 position;
        varying vec3 normal;

        void main () {
            float dotProduct = dot(-normal, lightDirection);
            float intensity = 0.4 + 0.6 * clamp(dotProduct, 0.0, 1.0);
            vec3 rgb = faceColor.rgb * intensity;
            gl_FragColor = vec4(rgb, faceColor.a);
        }
    |]


projectionMatrix : Window.Size -> Mat4
projectionMatrix { width, height } =
    let
        fovY =
            30

        aspectRatio =
            toFloat width / toFloat height

        zNear =
            0.1

        zFar =
            100
    in
        Math.Matrix4.makePerspective fovY aspectRatio zNear zFar


cubeEntity : Frame3d -> Window.Size -> WebGL.Entity
cubeEntity cubeFrame windowSize =
    let
        uniforms =
            { projectionMatrix = projectionMatrix windowSize
            , modelMatrix = Frame3d.modelMatrix cubeFrame
            , viewMatrix = Frame3d.viewMatrix eyeFrame
            , lightDirection = Direction3d.toVec3 lightDirection
            , faceColor = Color.toVec4 faceColor
            }

        settings =
            [ WebGL.Settings.cullFace WebGL.Settings.back ]
    in
        WebGL.entityWith settings vertexShader fragmentShader cubeMesh uniforms



-- Interactivity


mousePositionToPoint : Mouse.Position -> Point2d
mousePositionToPoint mousePosition =
    Point2d ( toFloat mousePosition.x, toFloat mousePosition.y )


touchToPoint : Touch -> Point2d
touchToPoint touch =
    Point2d ( touch.clientX, touch.clientY )


init : ( Model, Cmd Msg )
init =
    let
        model =
            { cubeFrame = initialCubeFrame
            , dragPoint = Nothing
            , windowSize = Nothing
            }
    in
        ( model, Task.perform SetWindowSize Window.size )


dragAttributes : List (Attribute Msg)
dragAttributes =
    let
        onMouseDown pointToMsg =
            Events.on "mousedown" Mouse.position
                |> Attributes.map (mousePositionToPoint >> pointToMsg)

        onTouch touchEvent pointToMsg =
            SingleTouch.onSingleTouch touchEvent Touch.preventAndStop .touch
                |> Attributes.map (touchToPoint >> pointToMsg)
    in
        [ onMouseDown StartRotatingAt
        , onTouch TouchStart StartRotatingAt
        , onTouch TouchMove PointerMovedTo
        , onTouch TouchEnd (always StopRotating)
        , onTouch TouchCancel (always StopRotating)
        ]


view : Model -> Html Msg
view model =
    case model.windowSize of
        Just windowSize ->
            let
                widthAttribute =
                    Attributes.width windowSize.width

                heightAttribute =
                    Attributes.height windowSize.height

                options =
                    [ WebGL.clearColor 0 0 0 1 ]

                attributes =
                    widthAttribute :: heightAttribute :: dragAttributes

                entities =
                    [ cubeEntity model.cubeFrame windowSize ]
            in
                WebGL.toHtmlWith options attributes entities

        Nothing ->
            Html.text ""


rotate : Frame3d -> Float -> Float -> Frame3d
rotate frame dx dy =
    let
        dragVector =
            Vector2d ( dx, dy )
    in
        case Vector2d.direction dragVector of
            Just direction2d ->
                let
                    axialDirection =
                        (Direction2d.perpendicularTo direction2d)
                            |> Direction2d.placeOnto SketchPlane3d.yz

                    rotationAxis =
                        Axis3d
                            { originPoint = Point3d.origin
                            , direction = axialDirection
                            }

                    rotationAngle =
                        degrees 1 * Vector2d.length dragVector
                in
                    frame |> Frame3d.rotateAround rotationAxis rotationAngle

            Nothing ->
                frame


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        StartRotatingAt startPoint ->
            ( { model | dragPoint = Just startPoint }, Cmd.none )

        StopRotating ->
            ( { model | dragPoint = Nothing }, Cmd.none )

        PointerMovedTo newPoint ->
            case model.dragPoint of
                Just lastPoint ->
                    let
                        ( dx, dy ) =
                            Vector2d.components
                                (Point2d.vectorFrom lastPoint newPoint)

                        rotatedFrame =
                            rotate model.cubeFrame dx -dy

                        updatedModel =
                            { model
                                | cubeFrame = rotatedFrame
                                , dragPoint = Just newPoint
                            }
                    in
                        ( updatedModel, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetWindowSize windowSize ->
            ( { model | windowSize = Just windowSize }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        dragEvents =
            case model.dragPoint of
                Just _ ->
                    Sub.batch
                        [ Mouse.moves (mousePositionToPoint >> PointerMovedTo)
                        , Mouse.ups (always StopRotating)
                        ]

                Nothing ->
                    Sub.none
    in
        Sub.batch [ dragEvents, Window.resizes SetWindowSize ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
