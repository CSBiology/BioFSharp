module BioFSharp.Parallel.Tests

open System
open NUnit.Framework
open BioFSharp
open BioFSharp.Algorithm
open BioFSharp.Algorithm.PairwiseAlignment
open BioFSharp.Parallel

let random = System.Random()
let randomSequence length =
    System.String [| for i in 0..length-1 -> [|'A'; 'C'; 'G'; 'T'|].[random.Next(4)] |]

let seq1 = randomSequence 500 |> BioArray.ofNucleotideString
let seq2 = randomSequence 500 |> BioArray.ofNucleotideString

let nucConversion (nucs:Nucleotides.Nucleotide list) =
    nucs |> List.map (fun element -> if string element = "Gap" then "-" else string element) |> String.Concat

let nucCosts =
    {
        Open = -5
        Continuation = -1
        Similarity = ScoringMatrix.getScoringMatrixNucleotide  ScoringMatrix.ScoringMatrixNucleotide.EDNA 
    }

let nucCostsPrimitive = new BioFSharp.Parallel.PairwiseAlignment.Costs(-5, -1, ScoringMatrix.getPrimitiveScoringMatrixNucleotide  ScoringMatrix.ScoringMatrixNucleotide.EDNA)

[<Test>]
let ``SmithWaterman: sequential equals parallel`` () =
    let sequentialAlignment = PairwiseAlignment.SmithWaterman.runNucleotide nucCosts seq1 seq2
    let sequentialAlignment = (sequentialAlignment.AlignedSequences.[0] |> nucConversion, sequentialAlignment.AlignedSequences.[1] |> nucConversion)
    let parallelAlignment = PairwiseAlignment.SmithWaterman.run nucCostsPrimitive seq1 seq2
    Assert.AreEqual(sequentialAlignment, parallelAlignment)

[<Test>]
let ``NeedlemanWunsch: sequential equals parallel`` () =
    let sequentialAlignment = PairwiseAlignment.NeedlemanWunsch.runNucleotide nucCosts seq1 seq2
    let sequentialAlignment = (sequentialAlignment.AlignedSequences.[0] |> nucConversion, sequentialAlignment.AlignedSequences.[1] |> nucConversion)
    let parallelAlignment = PairwiseAlignment.NeedlemanWunsch.run nucCostsPrimitive seq1 seq2
    Assert.AreEqual(sequentialAlignment, parallelAlignment)