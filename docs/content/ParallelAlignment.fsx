(*** hide ***)
#r "System.Configuration.dll"
#I "../../bin/BioFSharp.Parallel/net461"
#r "Alea.dll"
#r "BioFSharp.Parallel.dll"
#r "BioFSharp.dll"

(** 
<table class="HeadAPI">
<td class="Head"><h1>Parallel Pairwise Alignment</h1></td>
<td class="API">
    <a id="APILink" href="https://csbiology.github.io/BioFSharp/reference/biofsharp-parallel-pairwisealignment.html" >&#128194;View module documentation</a>
</td>
</table>
Pairwise Sequence alignment may be performed by making usage of the GPUs parallel compuatation capacities. Functions for this are located at `BioFSharp.Parallel`. The gapvalues are evaluated using the <b>affine</b> gap-score-model.
*)

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

(** 
First the costs for gaps and the score for matches/mismatches has to be set.
*)
// Set the open and continuation costs, and the scoring matrix.
let nucCostsPrimitive = new BioFSharp.Parallel.PairwiseAlignment.Costs(-5, -1, ScoringMatrix.getPrimitiveScoringMatrixNucleotide  ScoringMatrix.ScoringMatrixNucleotide.EDNA)

(**
The alignment can then be performed with the following functions:
*)

// Calculate the alignment
let alignmentSW = Parallel.PairwiseAlignment.SmithWaterman.run nucCostsPrimitive seq1 seq2
printfn "%A" alignmentSW

// Also works with Needleman Wunsch
let alignmentNM = Parallel.PairwiseAlignment.NeedlemanWunsch.run nucCostsPrimitive seq1 seq2
printfn "%A" alignmentNM