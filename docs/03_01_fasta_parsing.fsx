(**
---
title: Fasta
category: BioParsers
categoryindex: 3
index: 0
---
*)

(*** hide ***)

(*** condition: prepare ***)
#r "nuget: FSharpAux, 1.1.0"
#r "nuget: FSharpAux.IO, 1.1.0"
#r "nuget: FSharp.Stats, 0.4.3"
#r "nuget: Plotly.NET, 2.0.0-preview.18"
#r "../src/BioFSharp/bin/Release/netstandard2.0/BioFSharp.dll"
#r "../src/BioFSharp.IO/bin/Release/netstandard2.0/BioFSharp.IO.dll"
#r "../src/BioFSharp.BioContainers/bin/Release/netstandard2.0/BioFSharp.BioContainers.dll"
#r "../src/BioFSharp.ML/bin/Release/netstandard2.0/BioFSharp.ML.dll"
#r "../src/BioFSharp.Stats/bin/Release/netstandard2.0/BioFSharp.Stats.dll"

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
# Fasta parsing

[![Binder]({{root}}img/badge-binder.svg)](https://mybinder.org/v2/gh/CSBiology/BioFSharp/gh-pages?filepath={{fsdocs-source-basename}}.ipynb)&emsp;
[![Script]({{root}}img/badge-script.svg)]({{root}}{{fsdocs-source-basename}}.fsx)&emsp;
[![Notebook]({{root}}img/badge-notebook.svg)]({{root}}{{fsdocs-source-basename}}.ipynb)

*Summary:* This example shows how to parse and write fasta formatted files with BioFSharp

One of the various biology-associated file formats that can be manipulated using BioFSharp is the Fasta format.
The Fasta format can be used to represent sequences of amino acids or nucleotides written in single-letter code.

A sequence constists of two parts: The first line (Header) starting with a ">" is followed by a sequence identification code which should represent a unique description of the sequence. 
Subsequent lines contain the sequence itself, which is separated into chunks of 60 to 80 characters per line.
For further information about the format please visit [NCBI - FASTA](https://blast.ncbi.nlm.nih.gov/Blast.cgi?CMD=Web&PAGE_TYPE=BlastDocs&DOC_TYPE=BlastHelp).

**Example:**

```txt
>sp|P19532| ribosomal protein L20 GN=rpl20 PE=rpl20.p01
MTRVKRGNVSRKRHKKILNMSKGFRGAASTLFRTANQQNMKALRYSYRNRRQKKRDFRRM
WITRVNSAVRRYGLNYSEFMNYLKTHKIQLNRKVIAQLSICDPEAFMQLLLF*
```

## Reading Fasta files
*)

open BioFSharp
open BioFSharp.IO

let fileDir = __SOURCE_DIRECTORY__ + "/data/"  //FASTAExample1.fasta"

//reads from file to an array of FastaItems.
let sequences = 
    fileDir + "Chlamy_Cp.fastA"
    |> FastA.fromFile BioArray.ofAminoAcidString
    |> Seq.toArray

sequences.[0]
(***include-it***)

(** 
Analogously it is possible to directly read compressed fastA files:
*)
let sequences2 = 
    fileDir + "Chlamy_Cp.gz"
    |> FastA.fromGzipFile BioArray.ofAminoAcidString
    

(**

In both cases it is worth noticing that `BioArray.ofAminoAcidString` can be replaced by any converter function.
The converter maps from the sequence of character to either amino acid or nucleotide sequences. Therefore use `BioArray.ofAminoAcidString` for petide and `BioArray.ofNucleotideString` for gene FastA files, respectively.
It is of course also possible to introduce any converter function.

## Writing FastA files

In order to write a collection of sequences (`FastaItem<_>`) into a file use the following function.
*)

(*** do-not-eval ***)
sequences
|> FastA.write BioItem.symbol (fileDir + "FASTAExample3.fasta")  


(**
## Example: protein length distribution

With the FastA reader it is straightforward to access the protein length distribution:
*)
open Plotly.NET

let sequencesLength = 
    fileDir + "Chlamy_Cp.fastA"
    |> FastA.fromFile BioArray.ofAminoAcidString
    |> Seq.map (fun item -> item.Sequence.Length)

let distChart =
    Chart.Histogram sequencesLength
    |> Chart.withXAxisStyle("length")
    |> Chart.withYAxisStyle("#count")

(*** condition: ipynb ***)
#if IPYNB
distChart
#endif // IPYNB

(***hide***)
distChart |> GenericChart.toChartHTML
(***include-it-raw***)




