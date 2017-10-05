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

let private unionRules intersectionType =
    match intersectionType with
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
    if hitA then
        match (inA, inB) with
            | (true,true)   -> ABleaveA
            | (false,true)  -> BIntoAB
            | (true,false)  -> AIntoOutside
            | (false,false) -> OutsideIntoA
    else
        match (inA, inB) with
        | (true,true)   -> ABleaveB
        | (false,true)  -> BIntoOutside
        | (true,false)  -> AIntoAB
        | (false,false) -> OutsideIntoB

let constructedSolid (rules:CsgRules) (a:Solid) (b: Solid) r  =
    let tuplePush a b = b,a
    let aIntersections = a r |> Seq.map (tuplePush true) 
    let bIntersections = b r |> Seq.map (tuplePush false)  
    let merged = (Seq.concat [aIntersections;bIntersections])   
                    |> Seq.sortBy (fun v->(fst v).t) 
                    |> Seq.toList 
    let rec iterate insideA insideB mergedList =  
        match mergedList with 
            | [] -> [] 
            | head::tail ->  
                let (intersection,hitA) = head 
                let intersectionType = getIntersectionType hitA insideA insideB
                let action = rules intersectionType
                let flipNormal i = { i with n=(-1.0*i.n)}
                let nextInA = (if (hitA) then not insideA else insideA)
                let nextInB = (if (not hitA) then not insideB else insideB)
                match action with
                    | Take    -> head::(iterate nextInA nextInB tail)
                    | Discard -> (iterate nextInA nextInB tail)
                    | Flip    -> (flipNormal intersection, hitA)::(iterate nextInA nextInB tail)
    iterate false false merged |> Seq.map fst 

let union = constructedSolid unionRules  
let subtract = constructedSolid subtractRules  
let intersect = constructedSolid intersectRules
let exclude = constructedSolid excludeRules

