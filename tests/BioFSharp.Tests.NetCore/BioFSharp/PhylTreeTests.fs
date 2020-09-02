module PhylTreeTests

open BioFSharp
open PhylTree
open BioList
open Nucleotides
open Expecto

let testPhylTree_oneGen = Node.Branch("ACTG", [])

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

let testMappingFun (n: Node<'n>) =
    match n with
        Branch(s, nl) -> s |> BioList.ofNucleotideString

[<Tests>]
let phylTreeTests =
    testList "PhylTree" [
        testCase "countLeafs" (fun() ->
            Expect.equal
                (testPhylTree_threeGens_string |> PhylTree.countLeafs)
                3
                "PhylTree.countLeafs did not return the correct number of leaves"
        )
    ]