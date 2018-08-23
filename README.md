# elm-3d-camera

This package is provides convenient ways to define and use perspective and
orthographic cameras in 3D. It is based on [`elm-geometry`](http://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest)
and has two main goals:

  - Provide a way to construct WebGL model/view/projection matrices that is more
    intuitive than using [`elm-explorations/linear-algebra`](http://package.elm-lang.org/packages/elm-explorations/linear-algebra/latest)
    directly
  - Provide standalone 3D-to-2D projection functionality that can be used
    outside of WebGL

## Defining cameras

To define a camera, you first create a viewpoint which represents the position
and orientation of the camera:

```elm
cameraViewpoint =
    Viewpoint3d.lookAt
        { eyePoint = Point3d.fromCoordinates ( 10, 0, 5 )
        , focalPoint = Point3d.origin
        , upDirection = Direction3d.positiveZ
        }
```

You can then create either a perspective or orthographic camera from the
viewpoint:

```elm
perspectiveCamera =
    Camera3d.perspective
        { viewpoint = cameraViewpoint
        , verticalFieldOfView = degrees 30
        , nearClipDistance = 0.1
        , farClipDistance = 1000
        , screenWidth = 1024
        , screenHeight = 768
        }

orthographicCamera =
    Camera3d.orthographic
        { viewpoint = cameraViewpoint
        , viewportHeight = 5
        , nearClipDistance = 0.1
        , farClipDistance = 1000
        , screenWidth = 1024
        , screenHeight = 768
        }
```

## WebGL rendering

Once you have a camera, you can use it to get WebGL model/view/projection
matrices:

```elm
projectionMatrix =
    Camera.projectionMatrix camera

viewMatrix =
    Camera.viewMatrix camera

modelViewProjectionMatrix =
    Camera3d.modelViewProjectionMatrix modelFrame camera
```

(In the final example, the position and orientation of the object to be
rendered is defined by an `elm-geometry` [`Frame3d`](http://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Frame3d)
value.)

## Projection to screen space

You can also use a `Camera3d` to project points, lines, triangles and polylines
from 3D to 2D:

```elm
import Point3d.Projection as Point3d
import LineSegment3d.Projection as LineSegment3d

point2d =
    point3d |> Point3d.toScreenSpace camera

lineSegment2d =
    lineSegment3d |> LineSegment3d.toScreenSpace camera
```

This allows you to, for example, do a perspective projection of 3D points and
lines into 2D so that those points and lines can be rendered with SVG (taking
advantage of SVG features like perfect circles and dashed lines which are
difficult to do with WebGL):

![Perspective projection](https://ianmackenzie.github.io/elm-3d-camera/1.0.0/projection.png)

## Roadmap

Several more features are planned:

  - More `Viewpoint3d` constructors other than just `lookAt`
  - More 3D-to-2D projections (directions, axes)
  - Construction of 3D pick rays and cut planes from 2D screen points and lines

## Questions? Comments?

Please [open a new issue](https://github.com/ianmackenzie/elm-3d-camera/issues) if you
run into a bug, if any documentation is missing/incorrect/confusing, or if
there's a new feature that you would find useful. For general questions about
using `elm-3d-camera`, try:

  - Joining the **#geometry** or **#webgl** channels on the [Elm Slack](http://elmlang.herokuapp.com/),
    or sending me (**@ianmackenzie**) a message - even if you don't have any
    particular questions right now, it would be great to know what you're hoping
    to do with the package!
  - Posting to the [Elm Discourse](https://discourse.elm-lang.org/) forums
  - Or if you happen to be in the New York area, come on out to the
    [Elm NYC meetup](https://www.meetup.com/Elm-NYC/) =)

You can also find me on Twitter ([@ianemackenzie](https://twitter.com/ianemackenzie)),
where I occasionally post `elm-geometry`-related stuff like demos or new
releases. Have fun, and don't be afraid to ask for help!
