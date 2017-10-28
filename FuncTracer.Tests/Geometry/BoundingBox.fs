module BoundingBox.Tests
open BoundingBox
open Xunit
open Xunit.Abstractions
open SceneParser
open Ray
open Cube
open FsCheck.Xunit
open System.Collections.Generic

[<Fact>]
let ``BoundingBox.intersect returns true if ray intersects box`` () =
    let unitCubeAabb = {
        min=(Point(-0.5,-0.5,-0.5)); 
        max=(Point(0.5,0.5,0.5)); 
    }
    let ray = { o=(Point(-10.0,-10.0,-10.0)); d=(Vector(1.0,1.0,1.0))}
    Assert.True(intersects unitCubeAabb ray)

[<Fact>]
let ``BoundingBox.intersect returns false if ray misses`` () =
    let unitCubeAabb = {
        min=(Point(-0.5,-0.5,-0.5)); 
        max=(Point(0.5,0.5,0.5)); 
    }
    let ray = { o=(Point(0.0,0.0,-10.0)); d=(Vector (0.1, 10.0, 0.1))}
    Assert.False(intersects unitCubeAabb ray)