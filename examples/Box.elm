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
import Touch exposing (Touch, TouchEvent(..))
import SingleTouch
import Math.Vector3 exposing (Vec3)
import Math.Matrix4 exposing (Mat4)
import WebGL exposing (Mesh)
import WebGL.Settings
import Mouse
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)


type Msg
    = StartRotatingAt Point2d
    | PointerMovedTo Point2d
    | StopRotating


type alias Model =
    { boxFrame : Frame3d
    , lastRotationPoint : Maybe Point2d
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
    }


type alias Varyings =
    { position : Vec3
    , normal : Vec3
    }


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


eyeFrame : Frame3d
eyeFrame =
    Frame3d
        { originPoint = Point3d ( 50, 0, 0 )
        , xDirection = Direction3d.y
        , yDirection = Direction3d.z
        , zDirection = Direction3d.x
        }


viewportWidth : Int
viewportWidth =
    1024


viewportHeight : Int
viewportHeight =
    768


init : ( Model, Cmd msg )
init =
    let
        initialFrame =
            Frame3d.xyz
                |> Frame3d.rotateAround Axis3d.z (degrees -30)
                |> Frame3d.rotateAround Axis3d.y (degrees 20)

        model =
            { boxFrame = initialFrame
            , lastRotationPoint = Nothing
            }
    in
        ( model, Cmd.none )


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
        varying vec3 position;
        varying vec3 normal;

        void main () {
            float intensity = 0.4 + 0.6 * clamp(dot(-normal, lightDirection), 0.0, 1.0);
            vec3 color = vec3(0.2, 0.3, 0.9) * intensity;
            gl_FragColor = vec4(color, 1.0);
        }
    |]


projectionMatrix : Mat4
projectionMatrix =
    Math.Matrix4.makePerspective
        30
        (toFloat viewportWidth / toFloat viewportHeight)
        0.1
        100


lightDirection : Direction3d
lightDirection =
    Vector3d ( -1, -1, -2 )
        |> Vector3d.direction
        |> Maybe.withDefault (Direction3d.flip Direction3d.z)


mousePositionToPoint : Mouse.Position -> Point2d
mousePositionToPoint { x, y } =
    Point2d ( toFloat x, toFloat y )


mousePositionDecoder : Decoder Point2d
mousePositionDecoder =
    Mouse.position |> Decode.map mousePositionToPoint


touchToPoint : Touch -> Point2d
touchToPoint { clientX, clientY } =
    Point2d ( clientX, clientY )


view : Model -> Html Msg
view model =
    let
        uniforms =
            { projectionMatrix = projectionMatrix
            , modelMatrix = Frame3d.modelMatrix model.boxFrame
            , viewMatrix = Frame3d.viewMatrix eyeFrame
            , lightDirection = Direction3d.toVec3 lightDirection
            }
    in
        WebGL.toHtmlWith [ WebGL.clearColor 0 0 0 1 ]
            [ Attributes.width viewportWidth
            , Attributes.height viewportHeight
            , Attributes.style [ ( "display", "block" ) ]
            , Events.on "mousedown"
                (mousePositionDecoder |> Decode.map StartRotatingAt)
            , SingleTouch.onSingleTouch TouchStart
                Touch.preventAndStop
                (.touch >> touchToPoint >> StartRotatingAt)
            , SingleTouch.onSingleTouch TouchMove
                Touch.preventAndStop
                (.touch >> touchToPoint >> PointerMovedTo)
            , SingleTouch.onSingleTouch TouchEnd
                Touch.preventAndStop
                (always StopRotating)
            , SingleTouch.onSingleTouch TouchCancel
                Touch.preventAndStop
                (always StopRotating)
            ]
            [ WebGL.entityWith [ WebGL.Settings.cullFace WebGL.Settings.back ]
                vertexShader
                fragmentShader
                cubeMesh
                uniforms
            ]


rotate : Frame3d -> Float -> Float -> Frame3d
rotate currentFrame dx dy =
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
                    currentFrame
                        |> Frame3d.rotateAround rotationAxis rotationAngle

            Nothing ->
                currentFrame


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        StartRotatingAt startPoint ->
            ( { model | lastRotationPoint = Just startPoint }, Cmd.none )

        StopRotating ->
            ( { model | lastRotationPoint = Nothing }, Cmd.none )

        PointerMovedTo newPoint ->
            case model.lastRotationPoint of
                Just lastPoint ->
                    let
                        ( dx, dy ) =
                            Vector2d.components
                                (Point2d.vectorFrom lastPoint newPoint)

                        rotatedFrame =
                            rotate model.boxFrame dx -dy

                        updatedModel =
                            { boxFrame = rotatedFrame
                            , lastRotationPoint = Just newPoint
                            }
                    in
                        ( updatedModel, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.lastRotationPoint of
        Just _ ->
            Sub.batch
                [ Mouse.moves (mousePositionToPoint >> PointerMovedTo)
                , Mouse.ups (always StopRotating)
                ]

        Nothing ->
            Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
