(*** hide ***)
#I "../../bin"
(**

# Pairwise Alignment
<a id="SourceCode" href="https://github.com/CSBiology/BioFSharp/blob/master/src/BioFSharp/Algorithm/PairwiseAlignment.fs">&lt;/&gt;view source code</a>
<a id="Author" href="https://github.com/HLWeil">&#128366;view author of this tutorial</a>
<br><br>

In this short tutorial, the usage of the Pairwise alignment-implementation is demonstrated. 
For global alignments, the <b>NeedlemanWunsch</b>-algorithm is used. For local alignments, the <b>SmithWaterman</b>-algorithm is used.
For both implementations, the gapvalues are evaluated using the <b>affine</b> gapscoremodel. 
*)
#r "BioFSharp.dll"
open BioFSharp
open BioFSharp.Algorithm
open PairwiseAlignment
open RunGeneric
open NeedlemanWunsch
open SmithWaterman

(**
<div class="box">
## Aligning aminoacid- and nucleotide-sequences


For defining the scores of matching and missmatching characters, the <b>scoring</b>-function is defined. In the case of Aminoacid- or Nucleotide-sequence-alignments, the integrated substitution-matrices can be used.
*)

//For aminoacids
let aaScoring = ScoringMatrix.getScoringMatrixAminoAcid ScoringMatrix.ScoringMatrixAminoAcid.BLOSUM62

//For nucleotides
let nucScoring = ScoringMatrix.getScoringMatrixNucleotide  ScoringMatrix.ScoringMatrixNucleotide.EDNA

(**

In order to align two sequences, not only the values for substitutions, but also the <b>costs</b> for gaps are needed. In this implementation, an affine gap-penalty is realized. An affine gap penalty weights continuations of gaps different than the opening of a gap. 
The scoring function (in this case a substitution matrix) and the two gap penalty values are combined into the <b>Costs</b>-type.

*)

//For aminoacids
let aaCosts = {
    Open = -5
    Continuation = -2
    Similarity = aaScoring 
    }

//For nucleotides
let nucCosts = {
    Open = -2
    Continuation = -1
    Similarity = nucScoring 
    }

(**

The alignment functions use <b>Arrays</b> as input. The Elements can be of any type, but require type equality. Also they need to have type equality with the input of the scoring function.
Both the global and local alignment algorithms take the same parameters (costs,firstSequence,secondSequence) and return the same format.

*)

//For aminoacids
let aaSeq1 = [|'A';'B';'D';'M'|] |> Array.map AminoAcidSymbols.aminoAcidSymbol
let aaSeq2 = [|'M';'A';'A';'B';'E';'D';'M'|] |> Array.map AminoAcidSymbols.aminoAcidSymbol

let globalAAAlignment = needlemanWunsch aaCosts aaSeq1 aaSeq2
let localAAAlignment = smithWaterman aaCosts aaSeq1 aaSeq2

//For nucleotides
let nucSeq1 = [|'A';'T';'G';'A'|] |> BioArray.ofNucleotideString
let nucSeq2 = [|'B';'A';'T';'V';'A';'W';'G'|] |> BioArray.ofNucleotideString

let globalNucAlignment = needlemanWunsch nucCosts nucSeq1 nucSeq2
let localNucAlignment = smithWaterman nucCosts nucSeq1 nucSeq2

(**
</div>*)

(**
<div class="box">
## Aligning anything else


This implementation was aimed to be as generic as possible. To achieve this, the scoring function can be designed at will, the only constraints being the need for two input variables and the type equality.

For example, one could use a simple "if else" equality function to match nucleotides or Integers.
*)

let scoring a b = 
    if a = b then 2
    else -2

let costs = {
    Open = -2
    Continuation = -1
    Similarity = scoring
    }


let globalAlignmentNuc = needlemanWunsch costs nucSeq1 nucSeq2
let localAlignmentNuc = smithWaterman costs nucSeq1 nucSeq2


let intSeq1 = [|1;2;3;4;5|]
let intSeq2 = [|1;1;2;4;6;7;|]

let globalAlignmentInt = needlemanWunsch costs intSeq1 intSeq2
let localAlignmentInt = smithWaterman costs intSeq1 intSeq2

(**
</div>*)
