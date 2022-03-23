module PairwiseAlignmentTests

open BioFSharp
open BioFSharp.Algorithm
open BioFSharp.Algorithm.PairwiseAlignment
open Expecto 

let aaScoring = ScoringMatrix.getScoringMatrixAminoAcid ScoringMatrix.ScoringMatrixAminoAcid.BLOSUM62
let nucScoring = ScoringMatrix.getScoringMatrixNucleotide  ScoringMatrix.ScoringMatrixNucleotide.EDNA

//For aminoacids
let costAA = {
    Open = 5
    Continuation = -1
    Similarity = aaScoring 
    }

//For nucleotides
let costN = {
    Open = -5
    Continuation = -1
    Similarity = nucScoring 
    }

let query1AA = "NLFVAAAAQTKNGQGWVPSNYITPVNSAAA" |> BioArray.ofAminoAcidSymbolString
let query2AA = "NLFVALYDFVASGDNTLSITKGEKLRVLGYNHNGEWCEAQTKNGQGWVPSNYITPVNS" |> BioArray.ofAminoAcidSymbolString

let localAA = 
    PairwiseAlignment.Local.SmithWaterman.align(query1AA,query2AA,costAA)

let globalAA =
    PairwiseAlignment.Global.NeedlemanWunsch.align(query1AA,query2AA,costAA)

let expectedLocalAA =
    [
        "AQTKNGQGWVPSNYITPVNS" |> BioList.ofAminoAcidSymbolString
        "AQTKNGQGWVPSNYITPVNS" |> BioList.ofAminoAcidSymbolString
    ]


//reuslt of https://www.ebi.ac.uk/Tools/psa/emboss_needle/
let expectedGlobalAA =
    [
        "--------------------------------NLFVAAAAQTKNGQGWVPSNYITPVNSAAA" |> BioList.ofAminoAcidSymbolString
        "NLFVALYDFVASGDNTLSITKGEKLRVLGYNHN-GEWCEAQTKNGQGWVPSNYITPVNS---" |> BioList.ofAminoAcidSymbolString
    ]



[<Tests>]
let AlignmentTests =
    testList "PairwiseAlignment" [
        testList "AminoAcids" [
            testCase "Local.FirstAlignment" (fun _ ->
                Expect.equal 
                    (localAA.Sequences |> Seq.item 0 |> BioSeq.toString) 
                    (expectedLocalAA[0] |> BioSeq.toString)
                    ""
            )
            testCase "Local.SecondAlignment" (fun _ ->
                Expect.equal 
                    (localAA.Sequences |> Seq.item 1 |> BioSeq.toString) 
                    (expectedLocalAA[1] |> BioSeq.toString)
                    ""
            )
            testCase "Global.FirstAlignment" (fun _ ->
                Expect.equal 
                    (globalAA.Sequences |> Seq.item 0 |> BioSeq.toString) 
                    (expectedGlobalAA[0] |> BioSeq.toString)
                    ""
            )
            testCase "Global.SecondAlignment" (fun _ ->
                Expect.equal 
                    (globalAA.Sequences |> Seq.item 1 |> BioSeq.toString) 
                    (expectedGlobalAA[1] |> BioSeq.toString)
                    ""
            )
        ]
    ]