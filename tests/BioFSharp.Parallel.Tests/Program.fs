//module BioFSharp.Parallel.Tests

open System
//open NUnit.Framework
open Alea
open System.Configuration
open BioFSharp
open BioFSharp.Algorithm
open BioFSharp.Algorithm.PairwiseAlignment
open BioFSharp.Parallel

Alea.Settings.Instance.Resource.AssemblyPath <- __SOURCE_DIRECTORY__ + @"\..\..\packages\Alea\tools\"
Alea.Settings.Instance.Resource.Path <- __SOURCE_DIRECTORY__ + @"..\..\bin"

let random = System.Random()
let randomSequence length =
    System.String [| for i in 0..length-1 -> [|'A'; 'C'; 'G'; 'T'|].[random.Next(4)] |]

let seq1 = randomSequence 2500 |> BioArray.ofNucleotideString
let seq2 = randomSequence 1750 |> BioArray.ofNucleotideString

let nucCosts =
    {
        Open = -5
        Continuation = -1
        Similarity = ScoringMatrix.getScoringMatrixNucleotide  ScoringMatrix.ScoringMatrixNucleotide.EDNA 
    }

let nucCostsPrimitive = new BioFSharp.Parallel.PairwiseAlignment.Costs(-5, -1, ScoringMatrix.getPrimitiveScoringMatrixNucleotide  ScoringMatrix.ScoringMatrixNucleotide.EDNA)

//[<Test>]
let SW () =
    printfn "\nTesting Smith Waterman..."

    let t = System.Diagnostics.Stopwatch.StartNew()
    let sequentialAlignment = PairwiseAlignment.SmithWaterman.runNucleotide nucCosts seq1 seq2
    t.Stop()
    printfn "execution time of sequential: %A" t.Elapsed
    
    let t = System.Diagnostics.Stopwatch.StartNew()
    let parallelAlignment = PairwiseAlignment.SmithWaterman.run nucCostsPrimitive seq1 seq2
    t.Stop()
    printfn "execution time of parallel: %A" t.Elapsed
    
    printfn "sequential equals parallel: %A" (sequentialAlignment = parallelAlignment)
    //Assert.AreEqual(sequentialAlignment, parallelAlignment)

//[<Test>]
let NW () =
    printfn "\nTesting Needleman Wunsch..."

    let t = System.Diagnostics.Stopwatch.StartNew()
    let sequentialAlignment = PairwiseAlignment.NeedlemanWunsch.runNucleotide nucCosts seq1 seq2
    t.Stop()
    printfn "execution time of sequential: %A" t.Elapsed
    
    let t = System.Diagnostics.Stopwatch.StartNew()
    let parallelAlignment = PairwiseAlignment.NeedlemanWunsch.run nucCostsPrimitive seq1 seq2
    t.Stop()
    printfn "execution time of parallel: %A" t.Elapsed
    
    printfn "sequential equals parallel: %A" (sequentialAlignment = parallelAlignment)
    //Assert.AreEqual(sequentialAlignment, parallelAlignment)

[<EntryPoint>]
let main argv = 
    printfn "First GPU execution always takes about an extra second."
    SW ()
    NW ()
    0