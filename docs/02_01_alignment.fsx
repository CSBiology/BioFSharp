(**
---
title: Alignment
category: Algorithms
categoryindex: 2
index: 1
---
*)

(*** hide ***)

(*** condition: prepare ***)
#r "nuget: FSharpAux, 1.1.0"
#r "nuget: FSharpAux.IO, 1.1.0"
#r "nuget: FSharp.Stats, 0.4.3"
#r "nuget: Plotly.NET, 2.0.0-preview.18"
#r "../bin/BioFSharp/netstandard2.0/BioFSharp.dll"
#r "../bin/BioFSharp.IO/netstandard2.0/BioFSharp.IO.dll"
#r "../bin/BioFSharp.BioContainers/netstandard2.0/BioFSharp.BioContainers.dll"
#r "../bin/BioFSharp.ML/netstandard2.0/BioFSharp.ML.dll"
#r "../bin/BioFSharp.Stats/netstandard2.0/BioFSharp.Stats.dll"

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: FSharpAux, 1.1.0"
#r "nuget: FSharpAux.IO, 1.1.0"
#r "nuget: FSharp.Stats, 0.4.3"
#r "nuget: Plotly.NET, 2.0.0-preview.18"
#r "nuget: Plotly.NET.Interactive, 2.0.0-preview.18"
#r "nuget: BioFSharp, {{fsdocs-package-version}}"
#r "nuget: BioFSharp.IO, {{fsdocs-package-version}}"
#r "nuget: BioFSharp.BioContainers, {{fsdocs-package-version}}"
#r "nuget: BioFSharp.ML, {{fsdocs-package-version}}"
#r "nuget: BioFSharp.Stats, {{fsdocs-package-version}}"
#endif // IPYNB

(**
# Pairwise Alignment

[![Binder]({{root}}img/badge-binder.svg)](https://mybinder.org/v2/gh/plotly/Plotly.NET/gh-pages?filepath={{fsdocs-source-basename}}.ipynb)&emsp;
[![Script]({{root}}img/badge-script.svg)]({{root}}{{fsdocs-source-basename}}.fsx)&emsp;
[![Notebook]({{root}}img/badge-notebook.svg)]({{root}}{{fsdocs-source-basename}}.ipynb)

*Summary:* This example shows how to perform sequence alignments in BioFSharp

In this short tutorial, the usage of the pairwise alignment implementation is demonstrated.
For global alignments, the **NeedlemanWunsch** algorithm is used. For local alignments, the **SmithWaterman** algorithm is used.

For both implementations, the gapvalues are evaluated using the **affine** gapscoremodel.

## Aligning aminoacid- and nucleotide-sequences

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

NeedlemanWunsch.runAminoAcidSymbol aaCosts aaSeq1 aaSeq2
(***include-it***)

SmithWaterman.runAminoAcidSymbol aaCosts aaSeq1 aaSeq2
(***include-it***)

//For nucleotides
let nucSeq1 = "ATGA" |> BioArray.ofNucleotideString
let nucSeq2 = "BATVAWG" |> BioArray.ofNucleotideString

NeedlemanWunsch.runNucleotide nucCosts nucSeq1 nucSeq2
(***include-it***)

SmithWaterman.runNucleotide nucCosts nucSeq1 nucSeq2
(***include-it***)

(**
## Aligning anything else

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

NeedlemanWunsch.runGeneric costs nucSeq1 nucSeq2
(***include-it***)
SmithWaterman.runGeneric costs nucSeq1 nucSeq2
(***include-it***)

(** 
or also Integers:
*)

let intSeq1 = [|1;2;3;4;5|]
let intSeq2 = [|1;1;2;4;6;7;|]

NeedlemanWunsch.runGeneric costs intSeq1 intSeq2
(***include-it***)

SmithWaterman.runGeneric costs intSeq1 intSeq2
(***include-it***)

(**
# Multiple sequence alignment with ClustalOmega

Clustal Omega is a multiple sequence alignment (MSA) tool. This tutorial describes using it in F# via the ClustalOWrapper. For some more indepth information about which parameters to choose for your goal, also check out [the official tutorial](http://www.clustal.org/omega/README).

## Aligning sequences from files

The first step is to create a wrapper-object. As optional input it takes a path to the clustalo executable you want to use. You have to fill this argument if you work with a precompiled verion or on linux.

you will have to [install clustal omega](http://www.clustal.org/omega/clustal-omega-1.2.2-win64.zip) yourself to use this wrapper.
*)

open BioFSharp.IO
open ClustalOWrapper
open BioFSharp

(**
```fsharp
let cw = ClustalOWrapper("path/where/you/extracted/clustal-omega/clustalo.exe")
```
*)

(***hide***)
let cw = ClustalOWrapper()

(**
The general structure of arguments the wrapper takes was kept the same as in the command line tool. In general, you need an `inputPath`, an `outputPath` and `parameters`. As there are several inputs possible, you have to choose what it is. As we want to align a normal sequence we just pick `SequenceFile`.
*)

let inputPath = Input.SequenceFile (__SOURCE_DIRECTORY__ + @"\data\Chlamy_Cp.fastA")

let outputPath = __SOURCE_DIRECTORY__ + @"\data\Chlamy_Cp.aln"

(**
As additional parameters go, we'll restrict input to FastA format and the output to Clustal format. Also we will use the `Force` parameter to force the overwrite of a possilby already existing file with the name `ChlamyCp.aln`.
*)
//Input has to be in FastA format
let inputModifier = Parameters.ClustalParams.Input [Parameters.InputCustom.Format Parameters.FileFormat.FastA]
//Output has to be in Clustal format
let outputModifier = Parameters.ClustalParams.Output [Parameters.OutputCustom.Format Parameters.FileFormat.Clustal]
//Forces overwriting
let forceModifier = Parameters.ClustalParams.Miscallaneous [Parameters.MiscallaneousCustom.Force]

(***do-not-eval***)
//Perform alignment
cw.AlignFromFile(inputPath,outputPath,[inputModifier;outputModifier;forceModifier])

(** 
## Aligning sequences directly in F# Interactive

With the `AlignSequences` method, one can also directly align sequences with the clustal tool and also directly receive the alignment directly in F#.

As input, it takes a collection of `TaggedSequence`s, and again a set of parameters. The default options can be used by not using any additional parameters.
*)

let sequences = [
    TaggedSequence.create "pep1" ("AAGECGEK")
    TaggedSequence.create "pep2" ("AAGEGEK")
    TaggedSequence.create "pep3" ("AAAGECGEK")
    TaggedSequence.create "pep4" ("AAGECGEL")
]

cw.AlignSequences(sequences,Seq.empty)
(*** include-it***)