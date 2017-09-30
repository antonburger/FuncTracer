module Geometry
open Ray
open Transform

type private CsgIntersectionType = 
    | OutsideIntoA 
    | OutsideIntoB 
    | BIntoAB 
    | AIntoAB 
    | ABleaveA 
    | ABleaveB 
    | AIntoOutside 
    | BIntoOutside

type private CsgIntersectionRule = Take | Discard | Flip

type private CsgRules = (CsgIntersectionType->CsgIntersectionRule)

let private unionRules intersectionType = match intersectionType with
        | OutsideIntoA -> Take
        | OutsideIntoB -> Take
        | AIntoOutside -> Take
        | BIntoOutside-> Take
        | _ -> Discard

let private subtractRules intersectionType = 
    match intersectionType with
        | OutsideIntoA -> Take
        | AIntoAB -> Flip
        | ABleaveB -> Flip
        | AIntoOutside -> Take
        | _ -> Discard

let private intersectRules intersectionType = 
    match intersectionType with
        | OutsideIntoA -> Discard
        | OutsideIntoB -> Discard
        | BIntoAB -> Take
        | AIntoAB -> Take
        | ABleaveA -> Take
        | ABleaveB  -> Take
        | AIntoOutside -> Discard
        | BIntoOutside -> Discard

let private excludeRules intersectionType = 
    match intersectionType with
        | OutsideIntoA -> Take
        | OutsideIntoB -> Take
        | BIntoAB -> Flip
        | AIntoAB -> Flip
        | ABleaveA -> Flip
        | ABleaveB  -> Flip
        | AIntoOutside -> Take
        | BIntoOutside -> Take

let private getIntersectionType hitA inA inB = 
    if hitA then match (inA, inB) with
            | (true,true)   -> ABleaveA
            | (false,true)  -> BIntoAB
            | (true,false)  -> AIntoOutside
            | (false,false) -> OutsideIntoA
    else match (inA, inB) with
            | (true,true)   -> ABleaveB
            | (false,true)  -> BIntoOutside
            | (true,false)  -> AIntoAB
            | (false,false) -> OutsideIntoB
type ConstructedSolid (rules:CsgRules, a:Intersectable, b: Intersectable)  =
    interface Intersectable with 
        member this.Intersect r = 
            let tuplePush a b = b,a
            let aIntersections = a.Intersect r |> Seq.map (tuplePush a) 
            let bIntersections = b.Intersect r |> Seq.map (tuplePush b)  
            let merged = (Seq.concat [aIntersections;bIntersections])   
                            |> Seq.sortBy (fun v->(fst v).t) 
                            |> Seq.toList 
            let rec iterate insideA insideB mergedList =  
                match mergedList with 
                    | [] -> [] 
                    | head::tail ->  
                        let (intersection,object) = head 
                        let hitA = object=a
                        let intersectionType = getIntersectionType hitA insideA insideB
                        let action = rules intersectionType
                        let flipNormal i = { i with n=(-1.0*i.n)}
                        let nextInA = (if (object=a) then not insideA else insideA)
                        let nextInB = (if (object=b) then not insideB else insideB)
                        match action with
                            | Take    -> head::(iterate nextInA nextInB tail)
                            | Discard -> (iterate nextInA nextInB tail)
                            | Flip    -> (flipNormal intersection, object)::(iterate nextInA nextInB tail)
            iterate false false merged |> Seq.map fst 

let union a b = ConstructedSolid(unionRules,a,b)
let subtract a b = ConstructedSolid(subtractRules,a,b)
let intersect a b = ConstructedSolid(intersectRules,a,b)
let exclude a b = ConstructedSolid(excludeRules,a,b)
