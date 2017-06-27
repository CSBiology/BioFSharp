(*** hide ***)
#I "../../bin"
(**

BioFSharp: DualAlignment
========================

In this short tutorial, the usage of the DualAlignment-algorithm is demonstrated. 
For global alignments, the <b>NeedlemanWunsch</b>-algorithm is used. For local alignments, the <b>SmithWaterman</b>-algorithm is used.
For both implementations, the gapvalues are evaluated using the <b>affine</b> gapscoremodel. 
*)
#r "BioFSharp.dll"
open BioFSharp
open BioFSharp.Algorithms
open DualAlignment
open NeedlemanWunsch
open SmithWaterman
open RunGeneric
(**
<div class="box">
Assigning gap and match-values
----------------
For defining the scores of matching and missmatching characters, the <b>scoring</b>-function is defined. Alternatevely, instead of match and missmatch, a scoring matrix could be used.

The scoring function gets included into the <b>costs</b>-variable. It also carries the <b>gap-penaltys</b>.

*)


let scm = ScoringMatrix.getScoringMatrixAminoAcid ScoringMatrix.ScoringMatrixAminoAcid.BLOSUM62
scm AminoAcidSymbols.AminoAcidSymbol.Ala AminoAcidSymbols.AminoAcidSymbol.Arg


let scoring a b = 
    if a = b then 2.
    else -2.

let costs = {
    Open = -2.
    Continuation = -1.
    Similarity = scoring
    }
(**
</div>*)

(**
<div class="box">
Input
----------------
The function uses <b>Arrays</b> as input. The Elements can be of any type, but require equality.
*)
let firstSequence = [|1;2;3;3;4;4;5|]
let secondSequence = [|1;2;3;4|] 
(**
</div>*)

(**
<div class="box">
Running the functions
----------------
Both algorithms take the same parameters (costs,firstSequence,secondSequence)
The returned value is a record type consisting of the alignment and its score.
*)
let globalAlignment = needlemanWunsch costs firstSequence secondSequence
let localAlignment = smithWaterman costs firstSequence secondSequence
(**
</div>*)
