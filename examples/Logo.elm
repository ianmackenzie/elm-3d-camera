module Logo exposing (edges, logoUnits, vertices)

import Frame3d exposing (Frame3d)
import LineSegment3d exposing (LineSegment3d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity(..))


type LogoUnits
    = LogoUnits


type Corner
    = Corner


type Center
    = Center


logoUnits : Float -> Quantity Float LogoUnits
logoUnits value =
    Quantity value


height : Float
height =
    0.9


xOffset : Float
xOffset =
    0.6


yOffset : Float
yOffset =
    0.6


zOffset : Float
zOffset =
    0.6


p0 : Point3d LogoUnits Corner
p0 =
    Point3d.origin


p1 : Point3d LogoUnits Corner
p1 =
    Point3d.fromTuple logoUnits ( 1, 0, 0 )


p2 : Point3d LogoUnits Corner
p2 =
    Point3d.fromTuple logoUnits ( 1, 1, 0 )


p3 : Point3d LogoUnits Corner
p3 =
    Point3d.fromTuple logoUnits ( 0, 1, 0 )


p4 : Point3d LogoUnits Corner
p4 =
    Point3d.fromTuple logoUnits ( 0, 1, height )


p5 : Point3d LogoUnits Corner
p5 =
    Point3d.fromTuple logoUnits ( 0, 0, height )


p6 : Point3d LogoUnits Corner
p6 =
    Point3d.fromTuple logoUnits ( 1, 0, height )


p7 : Point3d LogoUnits Corner
p7 =
    Point3d.fromTuple logoUnits ( 1, 1 - yOffset, height )


p8 : Point3d LogoUnits Corner
p8 =
    Point3d.fromTuple logoUnits ( 1, 1, height - zOffset )


p9 : Point3d LogoUnits Corner
p9 =
    Point3d.fromTuple logoUnits ( 1 - xOffset, 1, height )


centerFrame : Frame3d LogoUnits Corner { defines : Center }
centerFrame =
    Frame3d.atPoint (Point3d.fromTuple logoUnits ( 0.5, 0.5, height / 2 ))


vertices : List (Point3d LogoUnits Center)
vertices =
    [ p0, p1, p2, p3, p4, p5, p6, p7, p8, p9 ]
        |> List.map (Point3d.relativeTo centerFrame)


edges : List (LineSegment3d LogoUnits Center)
edges =
    [ LineSegment3d.from p0 p1
    , LineSegment3d.from p1 p2
    , LineSegment3d.from p2 p3
    , LineSegment3d.from p3 p0
    , LineSegment3d.from p0 p5
    , LineSegment3d.from p1 p6
    , LineSegment3d.from p2 p8
    , LineSegment3d.from p3 p4
    , LineSegment3d.from p5 p6
    , LineSegment3d.from p6 p7
    , LineSegment3d.from p7 p8
    , LineSegment3d.from p8 p9
    , LineSegment3d.from p7 p9
    , LineSegment3d.from p9 p4
    , LineSegment3d.from p4 p5
    ]
        |> List.map (LineSegment3d.relativeTo centerFrame)
