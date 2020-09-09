module PhylTreeTests

open BioFSharp
open PhylTree
open BioList
open Nucleotides
open Expecto

let testPhylTree_oneGen = Node.Branch("1", [])

let testPhylTree_threeGens_string =
    Node.Branch("ACTG",[
        Node.Branch("ACTT", [
            Node.Branch("ACTC", [])
        ])
        Node.Branch("ACGG", [
            Node.Branch("ACCG", [])
        ])
        Node.Branch("GCTG", [
            Node.Branch("TCTG", [])
        ])
    ])

let testPhylTree_threeGens_BioList =
    Node.Branch(BioList.ofNucleotideString "ACTG",[
        Node.Branch(BioList.ofNucleotideString "ACTT", [
            Node.Branch(BioList.ofNucleotideString "ACTC", [])
        ])
        Node.Branch(BioList.ofNucleotideString "ACGG", [
            Node.Branch(BioList.ofNucleotideString "ACCG", [])
        ])
        Node.Branch(BioList.ofNucleotideString "GCTG", [
            Node.Branch(BioList.ofNucleotideString "TCTG", [])
        ])
    ])

let testFoldFun (acc: string) (node: Node<'n>) =
    match node with
        Branch(s, nl) ->
            (s + "; " + acc)

let testMappingFun (n: Node<'n>) =
    match n with
        Branch(s, nl) -> s |> BioList.ofNucleotideString

[<Tests>]
let phylTreeTests =
    testList "PhylTree" [
        testCase "map" (fun() ->
            Expect.equal
                (PhylTree.map testMappingFun testPhylTree_threeGens_string)
                testPhylTree_threeGens_BioList
                "PhylTree.map did not return correct Node<'t>"
        )
        testCase "iter" (fun () ->
            let mutable testList = []
            let testIterFun (node: Node<'n>) =
                match node with
                    Branch (s, nl) -> do (testList <- testList @ [s])
            PhylTree.iter testIterFun testPhylTree_threeGens_string
            Expect.equal
                testList
                ["ACTG"; "ACTT"; "ACTC"; "ACGG"; "ACCG"; "GCTG"; "TCTG"]
                "PhylTree.iter did not return correct Node<'t>"
        )
        testCase "fold" (fun () ->
            let testAcc = ""
            Expect.equal
                (PhylTree.fold testAcc testFoldFun testPhylTree_threeGens_string)
                "ACTG; GCTG; TCTG; ACGG; ACCG; ACTT; ACTC; "
                "PhylTree.fold did not return correct accumulated value."
        )
        testCase "countLeafs" (fun () ->
            Expect.equal
                (testPhylTree_threeGens_string |> PhylTree.countLeafs)
                3
                "PhylTree.countLeafs did not return the correct number of leaves"
        )
        testCase "tryGetNodeBy" (fun () ->
            let testConditionFun (node: Node<'n>) =
                match node with
                    Branch(s, _) -> s = "ACTG"
            Expect.equal
                (PhylTree.tryGetNodeBy testConditionFun testPhylTree_threeGens_string)
                (Some testPhylTree_threeGens_string)
                "PhylTree.tryGetNodeBy did not return the correct Node<'n> for the given condition."
        )
    ]