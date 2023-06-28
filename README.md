## NOTE

[`elm-3d-scene`](http://package.elm-lang.org/packages/ianmackenzie/elm-3d-scene/latest)
has not yet been updated to use `elm-3d-camera` 4.x, so in the meantime to avoid version conflicts
you'll likely want to install `elm-3d-scene` before `elm-3d-camera`, e.g.

```
elm install ianmackenzie/elm-3d-scene
elm install ianmackenzie/elm-3d-camera
```

If you do run into a problem, try manually adjusting the version of `elm-3d-camera` in `elm.json` to
3.1.0 instead of 4.0.0.

# elm-3d-camera

This package provides convenient ways to define and use perspective and
orthographic cameras in 3D. It is based on [`elm-geometry`](http://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest)
and is used heavily by [`elm-3d-scene`](http://package.elm-lang.org/packages/ianmackenzie/elm-3d-scene/latest).
It can also be used standalone to:

  - Construct [WebGL](https://package.elm-lang.org/packages/elm-explorations/webgl/latest/)
    model/view/projection matrices in a way that is more intuitive than using
    [`elm-explorations/linear-algebra`](http://package.elm-lang.org/packages/elm-explorations/linear-algebra/latest)
    directly
  - Perform 3D-to-2D projection of various `elm-geometry` values (points, line segments, triangles).
    This in turn allows you to do things like render simple 3D shapes by projecting them into 2D so
    that they can be drawn with SVG instead of WebGL.
    

## Defining cameras

The functions in this package let you construct perspective or orthographic cameras in various
different ways, for example:

```elm
import Angle
import Camera3d
import Length
import Point3d

perspectiveCamera =
    Camera3d.lookAt
        { eyePoint = Point3d.meters 4 0 3
        , focalPoint = Point3d.origin
        , upDirection = Direction3d.positiveZ
        , projection = Camera3d.Perspective
        , fov = Camera3d.angle (Angle.degrees 30)
        }
```

Note that there are no functions for transforming (translating, rotating etc.) cameras - cameras are
intended to be 'throwaway' values that you would construct on the fly when doing some rendering. For
example, if in the above code you wanted to have an animated camera that tracked some moving object,
you might store the camera and object positions in your model as `Point3d` values but then recreate
the actual `Camera3d` value every frame.

## WebGL rendering

Once you have a camera, you can use it to get WebGL view and projection
matrices:

```elm
import WebGL.Matrices as WebGL

viewMatrix =
    WebGL.viewMatrix camera

projectionMatrix =
    WebGL.projectionMatrix camera
        { nearClipDepth = Length.meters 0.1
        , farClipDepth = Length.meters 100
        , aspectRatio = 16 / 9
        }
```

## Projection to screen space

You can also use a `Camera3d` to project points, lines, and triangles from 3D to 2D. This allows you
to, for example, do a perspective projection of 3D points and lines into 2D so that those points and
lines can be rendered with SVG (taking advantage of SVG features like perfect circles and dashed
lines which are difficult to do with WebGL):

![Perspective projection](https://ianmackenzie.github.io/elm-3d-camera/1.0.0/projection.png)

First, you must define the dimensions of the screen you want to project to; this should generally
be of the form

```elm
screen =
    Rectangle2d.from bottomLeftCorner topRightCorner
```

Note that if you want browser DOM coordinates directly, you'll probably want to use something like

```elm
scree =
    Rectangle2d.from
        (Point2d.pixels 0 clientHeight)
        (Point2d.pixels clientWidth 0)
```

since in HTML 0 is the top and positive Y is down. I personally generally prefer working in 2D
coordinate systems where positive Y is up (converting to DOM coordinates at the last possible
moment), so my code that projects from 3D to 2D looks like this:

```elm
import Point3d.Projection as Point3d
import LineSegment3d.Projection as LineSegment3d

screen =
    Rectangle2d.from Point2d.origin
        (Point2d.pixels 800 600)

point2d =
    point3d |> Point3d.toScreenSpace camera screen

lineSegment2d =
    lineSegment3d
        |> LineSegment3d.toScreenSpace camera screen
```

(The [`Overlay.elm`](https://github.com/ianmackenzie/elm-3d-camera/blob/master/examples/Overlay.elm)
example uses an under-development `Drawing2d` module which works in a coordinate system where
positive Y is up, converting to Y-down coordinates only when actually rendering to SVG internally.)

## Roadmap

A few more features are planned:

  - More 3D-to-2D projections (directions, axes)
  - Construction of 3D cut planes from 2D on-screen lines

## Questions? Comments?

Please [open a new issue](https://github.com/ianmackenzie/elm-3d-camera/issues) if you
run into a bug, if any documentation is missing/incorrect/confusing, or if
there's a new feature that you would find useful. For general questions about
using `elm-3d-camera`, try:

  - Joining the **#geometry** or **#webgl** channels on the [Elm Slack](http://elmlang.herokuapp.com/),
    or sending me (**@ianmackenzie**) a message - even if you don't have any
    particular questions right now, it would be great to know what you're hoping
    to do with the package!  - Posting to the [Elm Discourse](https://discourse.elm-lang.org/) forums

You can also find me on Twitter ([@ianemackenzie](https://twitter.com/ianemackenzie)),
where I occasionally post `elm-geometry`-related stuff like demos or new
releases. Have fun, and don't be afraid to ask for help!
