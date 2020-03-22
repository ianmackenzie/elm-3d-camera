module Teapot exposing (main)

import Angle
import Axis2d
import Axis3d exposing (Axis3d)
import Browser
import Browser.Dom
import WebGL.Matrices as WebGL
import Browser.Events
import Camera3d exposing (Camera3d)
import Direction2d exposing (Direction2d)
import Direction3d exposing (Direction3d)
import Frame2d
import Frame3d exposing (Frame3d)
import Geometry.Interop.LinearAlgebra.Direction3d as Direction3d
import Geometry.Interop.LinearAlgebra.Frame3d as Frame3d
import Geometry.Interop.LinearAlgebra.Point3d as Point3d
import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Meters, meters)
import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4)
import Pixels exposing (Pixels, inPixels, pixels)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity(..), zero)
import Rectangle2d
import SketchPlane3d exposing (SketchPlane3d)
import Task
import Vector2d exposing (Vector2d)
import Vector3d exposing (Vector3d)
import Viewpoint3d
import WebGL exposing (Mesh)



-- Types


type Msg
    = StartRotatingAt (Point2d Pixels TopLeftCoordinates)
    | PointerMovedTo (Point2d Pixels TopLeftCoordinates)
    | StopRotating
    | SetWindowSize WindowSize
    | LoadModel (Result Http.Error (Mesh Attributes))


type TopLeftCoordinates
    = TopLeftCoordinates


type WorldCoordinates
    = WorldCoordinates


type ModelCoordinates
    = ModelCoordinates


type ModelUnits
    = ModelUnits


modelUnits : Float -> Quantity Float ModelUnits
modelUnits value =
    Quantity value


type alias WindowSize =
    ( Quantity Float Pixels, Quantity Float Pixels )


type alias Model =
    { placementFrame : Frame3d ModelUnits WorldCoordinates { defines : ModelCoordinates }
    , mesh : () -> Maybe (Mesh Attributes)
    , dragPoint : Maybe (Point2d Pixels TopLeftCoordinates)
    , windowSize : Maybe WindowSize
    }


type alias Attributes =
    { position : Vec3
    , normal : Vec3
    }


type alias Uniforms =
    { modelMatrix : Mat4
    , viewMatrix : Mat4
    , projectionMatrix : Mat4
    , lightDirection : Vec3
    , faceColor : Vec3
    }


type alias Varyings =
    { interpolatedPosition : Vec3
    , interpolatedNormal : Vec3
    }



-- Constants


initialFrame : Frame3d ModelUnits WorldCoordinates { defines : ModelCoordinates }
initialFrame =
    Frame3d.atOrigin
        |> Frame3d.rotateAround Axis3d.z (Angle.degrees -30)
        |> Frame3d.rotateAround Axis3d.y (Angle.degrees 20)


lightDirection : Direction3d WorldCoordinates
lightDirection =
    Vector3d.meters -1 -1 -2
        |> Vector3d.direction
        |> Maybe.withDefault Direction3d.negativeZ


faceColor : Vec3
faceColor =
    vec3 0.2 0.3 0.9



-- Model loading


accumulateVertices : List (Quantity Float units) -> List (Point3d units coordinates) -> List (Point3d units coordinates)
accumulateVertices coordinates accumulated =
    case coordinates of
        x :: y :: z :: rest ->
            accumulateVertices rest
                (Point3d.xyz x y z :: accumulated)

        _ ->
            List.reverse accumulated


accumulateNormals : List Float -> List (Direction3d coordinates) -> List (Direction3d coordinates)
accumulateNormals components accumulated =
    case components of
        x :: y :: z :: rest ->
            accumulateNormals rest
                (Direction3d.unsafe { x = x, y = y, z = z } :: accumulated)

        _ ->
            List.reverse accumulated


accumulateFaces : List Int -> List ( Int, Int, Int ) -> List ( Int, Int, Int )
accumulateFaces indices accumulated =
    case indices of
        a :: b :: c :: d :: e :: f :: g :: h :: rest ->
            accumulateFaces rest (( b, c, d ) :: accumulated)

        _ ->
            List.reverse accumulated


meshDecoder : Decoder (Mesh Attributes)
meshDecoder =
    Decode.map3
        (\vertexData normalData faceData ->
            let
                frame =
                    Frame3d.atOrigin
                        |> Frame3d.rotateAround Axis3d.x (Angle.degrees 90)
                        |> Frame3d.translateIn Direction3d.negativeZ
                            (modelUnits 1)

                vertices =
                    accumulateVertices vertexData []
                        |> List.map (Point3d.placeIn frame)

                normals =
                    accumulateNormals normalData []
                        |> List.map (Direction3d.placeIn frame)

                faces =
                    accumulateFaces faceData []

                attributes =
                    List.map2
                        (\vertex normal ->
                            { position = Point3d.toVec3 vertex
                            , normal = Direction3d.toVec3 normal
                            }
                        )
                        vertices
                        normals
            in
            WebGL.indexedTriangles attributes faces
        )
        (Decode.field "vertices" (Decode.list (Decode.map modelUnits Decode.float)))
        (Decode.field "normals" (Decode.list Decode.float))
        (Decode.field "faces" (Decode.list Decode.int))



-- Rendering


camera : Camera3d ModelUnits WorldCoordinates
camera =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.lookAt
                { eyePoint = Point3d.xyz (modelUnits 15) zero zero
                , focalPoint = Point3d.origin
                , upDirection = Direction3d.positiveZ
                }
        , verticalFieldOfView = Angle.degrees 30
        }


vertexShader : WebGL.Shader Attributes Uniforms Varyings
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        uniform mat4 viewMatrix;
        uniform mat4 modelMatrix;
        uniform mat4 projectionMatrix;
        varying vec3 interpolatedPosition;
        varying vec3 interpolatedNormal;

        void main () {
          gl_Position = projectionMatrix * viewMatrix * modelMatrix * vec4(position, 1.0);
          interpolatedPosition = (modelMatrix * vec4(position, 1.0)).xyz;
          interpolatedNormal = (modelMatrix * vec4(normal, 0.0)).xyz;
        }
    |]


fragmentShader : WebGL.Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;
        uniform vec3 lightDirection;
        uniform vec3 faceColor;
        varying vec3 interpolatedPosition;
        varying vec3 interpolatedNormal;

        void main () {
            vec3 normal = normalize(interpolatedNormal);
            float dotProduct = dot(-normal, lightDirection);
            float intensity = 0.4 + 0.6 * clamp(dotProduct, 0.0, 1.0);
            gl_FragColor = vec4(faceColor * intensity, 1.0);
        }
    |]


entity : Mesh Attributes -> Frame3d ModelUnits WorldCoordinates { defines : ModelCoordinates } -> WindowSize -> WebGL.Entity
entity mesh placementFrame windowSize =
    let
        ( screenWidth, screenHeight ) =
            windowSize

        uniforms =
            { projectionMatrix =
                WebGL.projectionMatrix camera
                    { aspectRatio = Quantity.ratio screenWidth screenHeight
                    , nearClipDepth = modelUnits 1
                    , farClipDepth = modelUnits 100
                    }
            , modelMatrix = WebGL.modelMatrix placementFrame
            , viewMatrix = WebGL.viewMatrix (Camera3d.viewpoint camera)
            , lightDirection = Direction3d.toVec3 lightDirection
            , faceColor = faceColor
            }
    in
    WebGL.entity vertexShader fragmentShader mesh uniforms



-- Interactivity
-- mousePositionToPoint : Mouse.Position -> Point2d
-- mousePositionToPoint mousePosition =
--     Point2d.fromCoordinates ( toFloat mousePosition.x, toFloat mousePosition.y )


init : () -> ( Model, Cmd Msg )
init () =
    let
        model : Model
        model =
            { placementFrame = initialFrame
            , mesh = always Nothing
            , dragPoint = Nothing
            , windowSize = Nothing
            }

        cmds : Cmd Msg
        cmds =
            Cmd.batch
                [ Task.perform (\{ viewport } -> SetWindowSize ( pixels viewport.width, pixels viewport.height )) Browser.Dom.getViewport
                , Http.get { url = "teapot.json", expect = Http.expectJson LoadModel meshDecoder }
                ]
    in
    ( model, cmds )


positionDecoder : Decoder (Point2d Pixels TopLeftCoordinates)
positionDecoder =
    Decode.map2 Point2d.pixels
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)


dragAttributes : List (Attribute Msg)
dragAttributes =
    [ Events.on "mousedown" (Decode.map StartRotatingAt positionDecoder) ]


view : Model -> Html Msg
view model =
    case ( model.windowSize, model.mesh () ) of
        ( Just windowSize, Just mesh ) ->
            let
                blockAttribute =
                    Attributes.style "display" "block"

                ( width, height ) =
                    windowSize

                widthAttribute =
                    Attributes.width (round (inPixels width))

                heightAttribute =
                    Attributes.height (round (inPixels height))

                options =
                    [ WebGL.clearColor 0 0 0 1
                    , WebGL.depth 1
                    , WebGL.antialias
                    ]

                attributes =
                    blockAttribute :: widthAttribute :: heightAttribute :: dragAttributes

                entities =
                    [ entity mesh model.placementFrame windowSize ]
            in
            WebGL.toHtmlWith options attributes entities

        _ ->
            Html.text "Loading model..."


rotate : Frame3d units coordinates defines -> Vector2d Pixels TopLeftCoordinates -> Frame3d units coordinates defines
rotate frame dragVector =
    case Vector2d.direction (Vector2d.mirrorAcross Axis2d.x dragVector) of
        Just direction2d ->
            let
                axialDirection =
                    Direction3d.on SketchPlane3d.yz <|
                        Direction2d.perpendicularTo direction2d

                rotationAxis =
                    Axis3d.through Point3d.origin axialDirection

                rotationAngle =
                    Vector2d.length dragVector
                        |> Quantity.at
                            (Angle.degrees 1 |> Quantity.per (pixels 1))
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
                        rotatedFrame =
                            rotate model.placementFrame
                                (Vector2d.from lastPoint newPoint)

                        updatedModel =
                            { model
                                | placementFrame = rotatedFrame
                                , dragPoint = Just newPoint
                            }
                    in
                    ( updatedModel, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetWindowSize windowSize ->
            ( { model | windowSize = Just windowSize }, Cmd.none )

        LoadModel result ->
            case result of
                Ok mesh ->
                    ( { model | mesh = always (Just mesh) }, Cmd.none )

                Err _ ->
                    ( { model | mesh = always Nothing }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        dragEvents =
            case model.dragPoint of
                Just _ ->
                    Sub.batch
                        [ Browser.Events.onMouseMove (Decode.map PointerMovedTo positionDecoder)
                        , Browser.Events.onMouseUp (Decode.succeed StopRotating)
                        ]

                Nothing ->
                    Sub.none
    in
    Sub.batch
        [ dragEvents
        , Browser.Events.onResize (\width height -> SetWindowSize ( pixels (toFloat width), pixels (toFloat height) ))
        ]


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Teapot", body = [ view model ] }
        , update = update
        , subscriptions = subscriptions
        }
