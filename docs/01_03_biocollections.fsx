(**
---
title: BioCollections
category: BioFSharp Core
categoryindex: 1
index: 3
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
# BioCollections

[![Binder]({{root}}img/badge-binder.svg)](https://mybinder.org/v2/gh/plotly/Plotly.NET/gh-pages?filepath={{fsdocs-source-basename}}.ipynb)&emsp;
[![Script]({{root}}img/badge-script.svg)]({{root}}{{fsdocs-source-basename}}.fsx)&emsp;
[![Notebook]({{root}}img/badge-notebook.svg)]({{root}}{{fsdocs-source-basename}}.ipynb)

*Summary:* This example shows how to use collections of biological items in BioFSharp

Analogous to the build-in collections BioFSharp provides BioSeq, BioList and BioArray for individual collection specific optimized operations. 
The easiest way to create them are the `ofBioItemString` -functions
*)

open BioFSharp
open BioFSharp.IO

fsi.AddPrinter(FSIPrinters.prettyPrintBioCollection) // use pretty printer for bio collections

let s1 = "PEPTIDE" |> BioSeq.ofAminoAcidString 
let s2 = "PEPTIDE" |> BioList.ofAminoAcidString 
let s3 = "TAGCAT"  |> BioArray.ofNucleotideString 

///Peptide represented as a Bioseq
"PEPTIDE" |> BioSeq.ofAminoAcidString 
(***include-it***)

///Peptide represented as a BioList
"PEPTIDE"|> BioList.ofAminoAcidString 
(***include-it***)

///Nucleotide sequence represented as a BioArray
"TAGCAT" |> BioArray.ofNucleotideString 
(***include-it***)

(**
## Nucleotides

![Nucleotides1](img/Nucleotides.svg)

**Figure 1: Selection of covered nucleotide operations** (A) Biological principle. (B) Workflow with `BioSeq`. (C) Other covered functionalities.

Let's imagine you have a given gene sequence and want to find out what the according protein might look like.
*)
let myGene = BioArray.ofNucleotideString "ATGGCTAGATCGATCGATCGGCTAACGTAA"

(***hide***)
let myGenePrnt     = FSIPrinters.prettyPrintBioCollection myGene

(*** include-value:myGene ***)

(**
Yikes! Unfortunately we got the 5'-3' coding strand. For proper transcription we should get the complementary strand first:
*)

let myProperGene = BioArray.complement myGene

(*** include-value:myProperGene ***)

(**
Now let's transcribe and translate it:
*)

let myTranslatedGene = 
    myProperGene
    |> BioArray.transcribeTemplateStrand
    |> BioArray.translate 0

(*** include-value:myTranslatedGene ***)

(**
Of course, if your input sequence originates from the coding strand, you can directly transcribe it to mRNA since the 
only difference between the coding strand and the mRNA is the replacement of 'T' by 'U' (Figure 1B)
*)

let myTranslatedGeneFromCodingStrand = 
    myGene
    |> BioArray.transcribeCodingStrand
    |> BioArray.translate 0

(*** include-value:myTranslatedGeneFromCodingStrand ***)

(**
Other Nucleotide conversion operations are also covered:
*)

let mySmallGene      = BioSeq.ofNucleotideString  "ATGTTCCGAT"

let smallGeneRev     = BioSeq.reverse mySmallGene 

$"""Original:
{mySmallGene |> FSIPrinters.prettyPrintBioCollection}
Output
{smallGeneRev |> FSIPrinters.prettyPrintBioCollection}
"""
|> printfn "%s"
(***include-output***)

let smallGeneComp    = BioSeq.complement mySmallGene

$"""Original:
{mySmallGene |> FSIPrinters.prettyPrintBioCollection}
Output:
{smallGeneComp |> FSIPrinters.prettyPrintBioCollection}
"""
|> printfn "%s"
(***include-output***)

let smallGeneRevComp = BioSeq.reverseComplement mySmallGene

$"""Original:
{mySmallGene |> FSIPrinters.prettyPrintBioCollection}
Reverse:
{smallGeneRev |> FSIPrinters.prettyPrintBioCollection}
Output:
{smallGeneRevComp|> FSIPrinters.prettyPrintBioCollection}"""
|> printfn "%s"
(***include-output***)

(**

## AminoAcids

### Basics
Some functions which might be needed regularly are defined to work with nucleotides and amino acids:
*)

let myPeptide = "PEPTIDE" |> BioSeq.ofAminoAcidString 

(***hide***)
let myPeptidePrnt = FSIPrinters.prettyPrintBioCollection myPeptide

(*** include-value:myPeptide ***)

let myPeptideFormula = BioSeq.toFormula myPeptide |> Formula.toString 

(*** include-value:myPeptideFormula ***)

let myPeptideMass = BioSeq.toAverageMass myPeptide 

(*** include-value:myPeptideMass ***)

(**

### Digestion
BioFSharp also comes equipped with a set of tools aimed at cutting apart amino acid sequences. To demonstrate the usage, we'll throw some `trypsin` at the small RuBisCO subunit of _Arabidopos thaliana_:  
In the first step, we define our input sequence and the protease we want to use.
*)

let RBCS = 
    """MASSMLSSATMVASPAQATMVAPFNGLKSSAAFPATRKANNDITSITSNGGRVNCMQVWP
    PIGKKKFETLSYLPDLTDSELAKEVDYLIRNKWIPCVEFELEHGFVYREHGNSPGYYDGR
    YWTMWKLPLFGCTDSAQVLKEVEECKKEYPNAFIRIIGFDNTRQVQCISFIAYKPPSFT""" 
    |> BioArray.ofAminoAcidString

(***include-value:RBCS***)

let trypsin = Digestion.Table.getProteaseBy "Trypsin"

(**
With these two things done, digesting the protein is a piece of cake. For doing this, just use the `digest` function.  
*)
let digestedRBCS = Digestion.BioArray.digest trypsin 0 RBCS 

digestedRBCS
|> Array.map (fun x -> x.PepSequence |> FSIPrinters.prettyPrintBioCollection)
|> String.concat "\n"

(***include-it***)

(*
In reality, proteases don't always completely cut the protein down. Instead, some sites stay intact and should be considered for in silico analysis. 
This can easily be done with the `concernMissCleavages` function. It takes the minimum and maximum amount of misscleavages you want to have and also the digested protein. As a result you get all possible combinations arising from this information.
*)

let digestedRBCS' = Digestion.BioArray.concernMissCleavages 0 2 digestedRBCS

digestedRBCS'
|> Array.map (fun x -> x.PepSequence |> FSIPrinters.prettyPrintBioCollection)
|> String.concat "\n"

(***include-it***)