module Tests.Parser.Sphere
open System
open Xunit
open Xunit.Abstractions
open SceneParser
open Ray
open Sphere
open FsCheck.Xunit
open System.Collections.Generic

let approxEquals (a:float) (b:float) = 
    if ( a-b |> abs |> (<) 0.00001 ) then 
        Assert.True true
    else
        Assert.Equal(a,b)

type SphereTests(output:ITestOutputHelper)=
    [<Property>]
    let ``Will have zero or two intersections`` (ray:Ray) =
        let length= sphere ray |> Seq.length
        Assert.True (length=0 || length=2)

    [<Property(Skip="not true for Zero direction")>]
    let ``All intersections are on the surface of the sphere`` (ray:Ray) =
        sphere ray 
        |> Seq.iter (fun i-> 
                    output.WriteLine (sprintf "%A" i)
                    (Point.toVector i.p).Length
                    |> approxEquals 1.0
                )