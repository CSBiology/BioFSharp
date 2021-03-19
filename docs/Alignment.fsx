(*** hide ***)

(*** condition: prepare ***)
#r "nuget: Plotly.NET, 2.0.0-beta6"
#r "nuget: FSharpAux, 1.0.0"
#r "nuget: FSharpAux.IO, 1.0.0"
#r "nuget: FSharp.Stats, 0.4.0"
#r "../bin/BioFSharp/netstandard2.0/BioFSharp.dll"
#r "../bin/BioFSharp.IO/netstandard2.0/BioFSharp.IO.dll"
#r "../bin/BioFSharp.BioContainers/netstandard2.0/BioFSharp.BioContainers.dll"
#r "../bin/BioFSharp.ML/netstandard2.0/BioFSharp.ML.dll"
#r "../bin/BioFSharp.Stats/netstandard2.0/BioFSharp.Stats.dll"

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Plotly.NET, 2.0.0-beta6"
#r "nuget: FSharpAux, 1.0.0"
#r "nuget: FSharpAux.IO, 1.0.0"
#r "nuget: FSharp.Stats, 0.4.0"
#r "nuget: Plotly.NET.Interactive, 2.0.0-beta6"
#r "nuget: BioFSharp, {{fsdocs-package-version}}"
#r "nuget: BioFSharp.IO, {{fsdocs-package-version}}"
#r "nuget: BioFSharp.BioContainers, {{fsdocs-package-version}}"
#r "nuget: BioFSharp.ML, {{fsdocs-package-version}}"
#r "nuget: BioFSharp.Stats, {{fsdocs-package-version}}"
#endif // IPYNB

(**

# Pairwise Alignment

In this short tutorial, the usage of the pairwise alignment implementation is demonstrated.
For global alignments, the **NeedlemanWunsch** algorithm is used. For local alignments, the **SmithWaterman** algorithm is used.

For both implementations, the gapvalues are evaluated using the **affine** gapscoremodel.
*)

(**
Aligning aminoacid- and nucleotide-sequences
--------------------------------------------
For defining the scores of matching and missmatching characters, the **scoring** function is defined. In the case of amino acid or nucleotide sequence alignments, the integrated substitution-matrices can be used.
*)
open BioFSharp
open BioFSharp.Algorithm
open PairwiseAlignment

//For aminoacids
let aaScoring = ScoringMatrix.getScoringMatrixAminoAcid ScoringMatrix.ScoringMatrixAminoAcid.BLOSUM62

//For nucleotides
let nucScoring = ScoringMatrix.getScoringMatrixNucleotide  ScoringMatrix.ScoringMatrixNucleotide.EDNA

(**

In order to align two sequences, not only the values for substitutions, but also the **costs** for gaps are needed. In this implementation, an affine gap-penalty is realized. An affine gap penalty weights continuations of gaps different than the opening of a gap.
The scoring function (in this case a substitution matrix) and the two gap penalty values are combined into the `Costs` type.

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

The alignment functions use `Arrays` as input. The Elements can be of any type, but require type equality. Also they need to have type equality with the input of the scoring function.
Both the global and local alignment algorithms take the same parameters (costs,firstSequence,secondSequence) and return the same format.

*)

//For aminoacids
let aaSeq1 = "ACDM" |> BioArray.ofAminoAcidSymbolString
let aaSeq2 = "MAACEDM" |> BioArray.ofAminoAcidSymbolString

let globalAAAlignment = NeedlemanWunsch.runAminoAcidSymbol aaCosts aaSeq1 aaSeq2
(*** include-value:globalAAAlignment ***)
let localAAAlignment = SmithWaterman.runAminoAcidSymbol aaCosts aaSeq1 aaSeq2
(*** include-value:localAAAlignment ***)


//For nucleotides
let nucSeq1 = "ATGA" |> BioArray.ofNucleotideString
let nucSeq2 = "BATVAWG" |> BioArray.ofNucleotideString

let globalNucAlignment = NeedlemanWunsch.runNucleotide nucCosts nucSeq1 nucSeq2
(*** include-value:globalNucAlignment ***)
let localNucAlignment = SmithWaterman.runNucleotide nucCosts nucSeq1 nucSeq2
(*** include-value:localNucAlignment ***)

(**
Aligning anything else
----------------------

This implementation was aimed to be as generic as possible. To achieve this, the scoring function can be designed at will, the only constraints being the need for two input variables and the type equality.  
Also besides the alignment functions which only take BioItems as input and represent the gaps by the appropriate gaps of type BioItem. There is also a generic alignment function `runGeneric` which returns `lists of options`, where None represents gaps. Therefore any input type can be used, given it matches the cost function.  

For example, one could use a simple `if .. then .. else` equality function to match nucleotides
*)

let scoring a b = 
    if a = b then 2
    else -2

let costs = {
    Open = -2
    Continuation = -1
    Similarity = scoring
    }

let globalAlignmentNuc = NeedlemanWunsch.runGeneric costs nucSeq1 nucSeq2
(*** include-value:globalAlignmentNuc ***)
let localAlignmentNuc = SmithWaterman.runGeneric costs nucSeq1 nucSeq2
(*** include-value:localAlignmentNuc ***)

(** 
or also Integers:
*)

let intSeq1 = [|1;2;3;4;5|]
let intSeq2 = [|1;1;2;4;6;7;|]

let globalAlignmentInt = NeedlemanWunsch.runGeneric costs intSeq1 intSeq2
(*** include-value:globalAlignmentInt ***)
let localAlignmentInt = SmithWaterman.runGeneric costs intSeq1 intSeq2
(*** include-value:localAlignmentInt ***)

