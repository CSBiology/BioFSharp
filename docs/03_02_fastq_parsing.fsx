(**
---
title: Fastq
category: BioParsers
categoryindex: 3
index: 2
---
*)

(*** hide ***)

(*** condition: prepare ***)
#r "nuget: Plotly.NET, 2.0.0-preview.8"
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
#r "nuget: Plotly.NET, 2.0.0-preview.8"
#r "nuget: FSharpAux, 1.0.0"
#r "nuget: FSharpAux.IO, 1.0.0"
#r "nuget: FSharp.Stats, 0.4.0"
#r "nuget: Plotly.NET.Interactive, 2.0.0-preview.8"
#r "nuget: BioFSharp, {{fsdocs-package-version}}"
#r "nuget: BioFSharp.IO, {{fsdocs-package-version}}"
#r "nuget: BioFSharp.BioContainers, {{fsdocs-package-version}}"
#r "nuget: BioFSharp.ML, {{fsdocs-package-version}}"
#r "nuget: BioFSharp.Stats, {{fsdocs-package-version}}"
#endif // IPYNB

(**
# Fastq parsing

[![Binder]({{root}}img/badge-binder.svg)](https://mybinder.org/v2/gh/plotly/Plotly.NET/gh-pages?filepath={{fsdocs-source-basename}}.ipynb)&emsp;
[![Script]({{root}}img/badge-script.svg)]({{root}}{{fsdocs-source-basename}}.fsx)&emsp;
[![Notebook]({{root}}img/badge-notebook.svg)]({{root}}{{fsdocs-source-basename}}.ipynb)

*Summary:* This example shows how to parse and write fastq formatted files with BioFSharp

This module allows to parse FASTQ format data with original 4-lines entries into this record type
*)

/// FastqItem record contains header, sequence, qualityheader, qualitysequence of one entry

type FastqItem<'a,'b> = {
    Header          : string
    Sequence        : 'a
    QualityHeader   : string
    QualitySequence : 'b      
}

(**
To be able to use this parser you need to define two converter functions, 
one example for each you can also find in our module, but you also may need to write your own.

If you have following possible values for quality sequence:

```txt
!""#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~
```
with Sanger format, that can encode a Phred quality score from 0 to 93 using ASCII 33 to 126, 
then you can use our converting function:
*)

/// get Phred quality score
let qualityConvertFn (string:string) =
    string.ToCharArray()
    |> Array.map (fun i -> int i - 33)

(**
And then you can easily use this module to read your FastQ file
*)
open BioFSharp
open BioFSharp.IO

let yourFastqFile = (__SOURCE_DIRECTORY__ + "/data/FastQtest.fastq")

let FastQSequence = 
    FastQ.fromFile BioArray.ofAminoAcidString qualityConvertFn yourFastqFile

(***include-value:FastQSequence***)