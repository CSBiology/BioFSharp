#r "System.Configuration.dll"
#I "../../bin"
#r "Alea.dll"
#r "BioFSharp.Parallel.dll"
#r "BioFSharp.dll"

open System

// sequential
open BioFSharp
open BioFSharp.Algorithm
open BioFSharp.Algorithm.PairwiseAlignment

// parallel
open Alea
open BioFSharp.Parallel
Alea.Settings.Instance.Resource.AssemblyPath <- __SOURCE_DIRECTORY__ + @"\..\..\packages\Alea\tools\"
Alea.Settings.Instance.Resource.Path <- __SOURCE_DIRECTORY__ + @"..\..\bin"

let random = System.Random()
let randomSequence length =
    let randomNucleotide () =
        [|'A'; 'C'; 'G'; 'T'|].[random.Next(4)]
    System.String [| for i in 0..length-1 -> randomNucleotide () |]

let executeSequential nucCosts seq1 seq2 =
    let nucConversion (nucs:Nucleotides.Nucleotide list) =
        nucs |> List.map (fun element -> if string element = "Gap" then "-" else string element) |> String.Concat
    let t = System.Diagnostics.Stopwatch.StartNew()
    let sequentialAlignment = PairwiseAlignment.SmithWaterman.runNucleotide nucCosts seq1 seq2
    t.Stop()
    printfn "execution time of sequential: %A" t.Elapsed
    (sequentialAlignment.AlignedSequences.[0] |> nucConversion, sequentialAlignment.AlignedSequences.[1] |> nucConversion)

let executeParallel nucCosts seq1 seq2 =
    let t = System.Diagnostics.Stopwatch.StartNew()
    let parallelAlignment = PairwiseAlignment.SmithWaterman.run nucCosts seq1 seq2
    t.Stop()
    printfn "execution time of parallel: %A" t.Elapsed
    parallelAlignment

let nucScoring = ScoringMatrix.getScoringMatrixNucleotide  ScoringMatrix.ScoringMatrixNucleotide.EDNA

let nucCosts = {
    Open = -5
    Continuation = -1
    Similarity = nucScoring 
    }

let seq1 = randomSequence 500 |> BioArray.ofNucleotideString
let seq2 = randomSequence 500 |> BioArray.ofNucleotideString

let sequentialAlignment = executeSequential nucCosts seq1 seq2
let parallelAlignment = executeParallel nucCosts seq1 seq2

printfn "sequential: %A" sequentialAlignment
printfn "parallel: %A" parallelAlignment
printfn "alignments equal: %A" (parallelAlignment = sequentialAlignment)