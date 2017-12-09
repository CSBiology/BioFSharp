#r "System.Configuration.dll"
#I "../../bin"
#r "Alea.dll"
#r "BioFSharp.Parallel.dll"
#r "BioFSharp.dll"

open System
open BioFSharp
open BioFSharp.Algorithm
open BioFSharp.Parallel
Alea.Settings.Instance.Resource.AssemblyPath <- __SOURCE_DIRECTORY__ + @"\..\..\packages\Alea\tools\"
Alea.Settings.Instance.Resource.Path <- __SOURCE_DIRECTORY__ + @"..\..\bin"

// Generate some random nucleotide sequences.
let random = System.Random()
let randomSequence length =
    System.String [| for i in 0..length-1 -> [|'A'; 'C'; 'G'; 'T'|].[random.Next(4)] |]

let seq1 = randomSequence 10 |> BioArray.ofNucleotideString
let seq2 = randomSequence 10 |> BioArray.ofNucleotideString

// Set the open and continuation costs, and the scoring matrix.
let nucCostsPrimitive = new BioFSharp.Parallel.PairwiseAlignment.Costs(-5, -1, ScoringMatrix.getPrimitiveScoringMatrixNucleotide  ScoringMatrix.ScoringMatrixNucleotide.EDNA)

// Calculate the alignment
let alignment = Parallel.PairwiseAlignment.NeedlemanWunsch.run nucCostsPrimitive seq1 seq2
printfn "%A" alignment