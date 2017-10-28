module Tests.Triangle

open System
open Xunit
open Xunit.Abstractions
open SceneParser
open Triangle
open FsCheck.Xunit
open System.Collections.Generic
open Plane

type TestTriangleSlice(output:ITestOutputHelper)=
    let a = Point(-1.0,0.0,0.0)
    let b = Point(1.0,2.0,0.0)
    let c = Point(1.0,0.0,0.0)
    let plane = (Plane(Point(0.0,0.0,0.0),Vector(-1.0,0.0,0.0)))

    let equals (a:Triangle) (b:Triangle) = Assert.Equal(a,b)

    let sliceExample a' b' c' = 
        let abIntercept = Point(0.0,1.0,0.0)
        let acIntercept = Point(0.0,0.0,0.0)
        let getTriangles (above,below) = (above|>Seq.head, below |> Seq.head, below|>Seq.skip 1 |> Seq.head)
        let sliced = slice plane (Triangle(a',b',c'))
        let (t1,t2,t3) =  sliced |> getTriangles
        Assert.Equal(Triangle(a,abIntercept, acIntercept), t1)
        Assert.Equal(Triangle(abIntercept, b, c),t2)
        Assert.Equal(Triangle(c, acIntercept, abIntercept), t3)

    [<Fact>]
    member __.``a triangle bisecting by the plane is split into 3 on either side of a plane `` () =
        sliceExample a b c

    [<Fact>]
    member __.``slice gives the same result regardless of point order, for the same orientation`` () = 
        sliceExample a b c
        sliceExample c a b 
        sliceExample b c a 

    [<Fact>]
    member __.``Triangles above the plane are returned unchanged in fst`` () = 
        let above = Triangle(
            Point(-1.0,0.0,0.0), 
            Point(-2.0,1.0,0.0), 
            Point(-1.0,1.0,0.0)
            )
        slice plane above |> fst |> Seq.head |> equals above

    [<Fact>]
    member __.``Triangles below the plane are returned unchanged in snd`` () = 
        let below = Triangle(
            Point(1.0,0.0,0.0), 
            Point(2.0,1.0,0.0), 
            Point(1.0,1.0,0.0)
            )
        slice plane below |> snd |> Seq.head |> equals below
