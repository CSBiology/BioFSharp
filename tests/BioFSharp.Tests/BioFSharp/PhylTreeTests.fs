module PhylTreeTests

open BioFSharp
open BioList
open Nucleotides
open Expecto

let testPhylTree_oneGen = Branch("1", [])

let testPhylTree_threeGens_string =
    Branch("ACTG",[
        Branch("ACTT", [
            Branch("ACTC", [])
        ])
        Branch("ACGG", [
            Branch("ACCG", [])
        ])
        Branch("GCTG", [
            Branch("TCTG", [])
        ])
    ])

let testPhylTree_threeGens_BioList =
    Branch(BioList.ofNucleotideString "ACTG",[
        Branch(BioList.ofNucleotideString "ACTT", [
            Branch(BioList.ofNucleotideString "ACTC", [])
        ])
        Branch(BioList.ofNucleotideString "ACGG", [
            Branch(BioList.ofNucleotideString "ACCG", [])
        ])
        Branch(BioList.ofNucleotideString "GCTG", [
            Branch(BioList.ofNucleotideString "TCTG", [])
        ])
    ])

let testFoldFun (acc: string) (tree: PhylogeneticTree<'n>) =
    match tree with
        Branch(s, nl) ->
            (s + "; " + acc)

let testMappingFun (tree: PhylogeneticTree<'n>) =
    match tree with
        Branch(s, nl) -> s |> BioList.ofNucleotideString

[<Tests>]
let phylTreeTests =
    testList "PhylTree" [
        testCase "map" (fun() ->
            Expect.equal
                (PhylogeneticTree.map testMappingFun testPhylTree_threeGens_string)
                testPhylTree_threeGens_BioList
                "PhylTree.map did not return correct Node<'t>"
        )
        testCase "iter" (fun () ->
            let mutable testList = []
            let testIterFun (node: PhylogeneticTree<'n>) =
                match node with
                    Branch (s, nl) -> do (testList <- testList @ [s])
            PhylogeneticTree.iter testIterFun testPhylTree_threeGens_string
            Expect.equal
                testList
                ["ACTG"; "ACTT"; "ACTC"; "ACGG"; "ACCG"; "GCTG"; "TCTG"]
                "PhylTree.iter did not return correct Node<'t>"
        )
        testCase "fold" (fun () ->
            let testAcc = ""
            Expect.equal
                (PhylogeneticTree.fold testAcc testFoldFun testPhylTree_threeGens_string)
                "ACTG; GCTG; TCTG; ACGG; ACCG; ACTT; ACTC; "
                "PhylTree.fold did not return correct accumulated value."
        )
        testCase "countLeafs" (fun () ->
            Expect.equal
                (testPhylTree_threeGens_string |> PhylogeneticTree.countLeafs)
                3
                "PhylTree.countLeafs did not return the correct number of leaves"
        )
        testCase "tryGetNodeBy" (fun () ->
            let testConditionFun (node: PhylogeneticTree<'n>) =
                match node with
                    Branch(s, _) -> s = "ACTG"
            Expect.equal
                (PhylogeneticTree.tryGetNodeBy testConditionFun testPhylTree_threeGens_string)
                (Some testPhylTree_threeGens_string)
                "PhylTree.tryGetNodeBy did not return the correct Node<'n> for the given condition."
        )
    ]