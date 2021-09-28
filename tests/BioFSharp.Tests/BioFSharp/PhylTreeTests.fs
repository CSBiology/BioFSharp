module PhylTreeTests

open BioFSharp
open BioList
open Nucleotides
open Expecto

let testPhylTree_oneGen = Branch("1", [])

let testPhylTree_threeGens_string =
    Branch("ACTG",[
        Branch("ACTT", [
            Leaf "ACTC"
        ])
        Branch("ACGG", [
            Leaf "ACCG"
        ])
        Branch("GCTG", [
            Leaf "TCTG"
        ])
    ])

let testPhylTree_threeGens_BioList =
    Branch(BioList.ofNucleotideString "ACTG",[
        Branch(BioList.ofNucleotideString "ACTT", [
            Leaf (BioList.ofNucleotideString "ACTC")
        ])
        Branch(BioList.ofNucleotideString "ACGG", [
            Leaf(BioList.ofNucleotideString "ACCG")
        ])
        Branch(BioList.ofNucleotideString "GCTG", [
            Leaf(BioList.ofNucleotideString "TCTG")
        ])
    ])

[<Tests>]
let phylTreeTestsBase =
    testList "PhylogeneticTree" [
        testCase "base functions.map" (fun() ->
            Expect.equal
                (PhylogeneticTree.map BioList.ofNucleotideString BioList.ofNucleotideString testPhylTree_threeGens_string)
                testPhylTree_threeGens_BioList
                "PhylTree.map did not return correct Node<'t>"
        )
        testCase "base functions.iter" (fun () ->
            let mutable testList = []
            let testIterFun = (fun x -> testList <- testList @ [x])

            PhylogeneticTree.iter testIterFun testIterFun testPhylTree_threeGens_string

            Expect.equal
                testList
                ["ACTG"; "ACTT"; "ACTC"; "ACGG"; "ACCG"; "GCTG"; "TCTG"]
                "PhylTree.iter did not return correct Node<'t>"
        )
        testCase "base functions.fold" (fun () ->
            Expect.equal
                (PhylogeneticTree.fold (fun acc elem -> if acc = "" then $"{elem}" else $"{acc}; {elem}") "" testPhylTree_threeGens_string)
                "ACTG; ACTT; ACTC; ACGG; ACCG; GCTG; TCTG"
                "PhylTree.fold did not return correct accumulated value."
        )
        testCase "base functions.countLeafs" (fun () ->
            Expect.equal
                (testPhylTree_threeGens_string |> PhylogeneticTree.countLeafs)
                3
                "PhylTree.countLeafs did not return the correct number of leaves"
        )        
        testCase "base functions.countBranches" (fun () ->
            Expect.equal
                (testPhylTree_threeGens_string |> PhylogeneticTree.countBranches)
                4
                "PhylTree.countBranches did not return the correct number of branches"
        )
    ]