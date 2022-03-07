(**
---
title: GFF3
category: BioParsers
categoryindex: 3
index: 2
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
# GFF3 parsing

[![Binder]({{root}}img/badge-binder.svg)](https://mybinder.org/v2/gh/plotly/Plotly.NET/gh-pages?filepath={{fsdocs-source-basename}}.ipynb)&emsp;
[![Script]({{root}}img/badge-script.svg)]({{root}}{{fsdocs-source-basename}}.fsx)&emsp;
[![Notebook]({{root}}img/badge-notebook.svg)]({{root}}{{fsdocs-source-basename}}.ipynb)

*Summary:* This example shows how to parse and write gff3 formatted files with BioFSharp

## Generic Feature Format Version 3

In GFF3 files every line represents one genomic feature with nine tab-delimited fields, whereas unlimited key-value pairs can be stored in field 9. 
It is possible to link multiple features to genomic units using the 'Parent tag'.

In the following you can see a GFF file example (modified version of [saccharomyces_cerevisiae.gff](http://downloads.yeastgenome.org/curation/chromosomal_feature/saccharomyces_cerevisiae.gff)):

```text
##gff-version 3
# date Mon Feb  7 19:35:06 2005
chrI	SGD	gene	335	649	.	+	.	ID=YAL069W;Name=YAL069W;Ontology_term=GO:0000004,GO:0005554,GO:0008372;Note=Hypothetical%20ORF;dbxref=SGD:S000002143;orf_classification=Dubious
chrI	SGD	CDS	335	649	.	+	0	Parent=YAL069W;Name=YAL069W;Ontology_term=GO:0000004,GO:0005554,GO:0008372;Note=Hypothetical%20ORF;dbxref=SGD:S000002143;orf_classification=Dubious
###
##FASTA
>chrI
CCACACCACACCCACACACCCACACACCACACCACACACCACACCACACCCACACACACACATCCTAACACTACCCTAAC
ACAGCCCTAATCTAACCCTGGCCAACCTGTCTCTCAACTTACCCTCCATTACCCTGCCTCCACTCGTTACCCTGTCCCAT
TCAACCATACCACTCCGAACCACCATCCATCCCTCTACTTACTACCACTCACCCACCGTTACCCTCCAATTACCCATATC
CAACCCACTGCCACTTACCCTACCATTACCCTACCATCCACCATGACCTACTCACCATACTGTTCTTCTACCCACCATAT
TGAAACGCTAACAAATGATCGTAAATAACACACACGTGCTTACCCTACCACTTTATACCACCACCACATGCCATACTCAC
CCTCACTTGTATACTGATTTTACGTACGCACACGGATGCTACAGTATATACCATCTCAAACTTACCCTACTCTCAGATTC
CACTTCACTCCATGGCCCATCTCTCACTGAATCAGTACCAAATGCACTCACATCATTATGCACGGCACTTGCCTCAGCGG
TCTATACCCTGTGCCATTTACCCATAACGCCCATCATTATCCACATTTTGATATCTATATCTCATTCGGCGGTCCCAAAT
ATTGTATAACTGCCCTTAATACATACGTTATACCACTTTTGCACCATATACTTACCACTCCATTTATATACACTTATGTC
AATATTACAGAAAAATCCCCACAAAAATCACCTAAACATAAAAATATTCTACTTTTCAACAATAATACATAAACATATTG
```

Directives (marked with "##[...]") provide additional information like the gff-version which has to be the first line of each file ("##gff-version 3[...]"). 
Comment lines have to start with a single "#[...]". It is possible that sequences in FastA format are attached at the end of the file. This has to be announced by a "##FASTA" directive line.

For further information visit [GFF3-Specifications](https://github.com/The-Sequence-Ontology/Specifications/blob/master/gff3.md).

## Reading GFF3 files


To read in a gff you have to insert a filepath and optionally a FastA converter. For further information about FastA check the [FastA section](https://csbiology.github.io/BioFSharp/FastA.html)
or visit [API Reference - FastA](https://csbiology.github.io/BioFSharp/reference/biofsharp-io-fasta.html).
*)
open BioFSharp
open BioFSharp.IO

//path of the input file
let filepathGFF = (__SOURCE_DIRECTORY__ + "path/to/your/gff3.gff")

//reads from file to seq of GFFLines
//If no FASTA Sequence is included you directly can use GFF3.fromFileWithoutFasta [filepathGFF].
(***do-not-eval***)
let features = GFF3.fromFile BioFSharp.BioArray.ofNucleotideString filepathGFF 



(**
## How to use GFF3SanityCheck


The GFF3SanityCheck prints wether your GFF3 file is valid or not. It returns all specified errors including the lines in which they occured.
In contrast to GFF2 the field 3 (type, feature or method) of a GFF3 entry is restricted to terms defined by the sequence ontology (SO) so this validator is able to check if the entry is a valid SO term.
You can find new versions of the SO at (https://sourceforge.net/projects/song/files/SO_Feature_Annotation).
*)

(***do-not-eval***)
//to validate the GFF file without SOTerm verification use this function and only insert the filepath
let featuresSanityCheck = GFF3.sanityCheck filepathGFF

(***do-not-eval***)
//path, name and version of the 'Sequence Ontology terms'-file
let filepathSO_Terms = (__SOURCE_DIRECTORY__ + "/data/Sequence_Ontology_Terms_2_5_3.txt")

(***do-not-eval***)
//to validate the gff file insert filepath
let featuresSanityCheckWithSOTerm = GFF3.sanityCheckWithSOTerm filepathSO_Terms filepathGFF


(**
## How to use GFF3RelationshipSearch


You also can do a simple search for "Parent - child of" relationships giving back all genomic features which contain the searchterm in **ID/Id** or **Parent** field.

*)

///Term to search for:
let searchterm = "YAL069W"

(***do-not-eval***)
///with this function you can search features which are related to the searchterm
let gffExampleSearch = GFF3.relationshipSearch features searchterm


(**
##Writing GFF3 files

In order to write a sequence of (GFFLine<_>) into a file use the following function.
If FastA sequences are included they are appended by a FastA writer described in the [API Reference - FastA](https://csbiology.github.io/BioFSharp/reference/biofsharp-io-fasta.html).

_Note: The order of key value pairs in field 9 (attributes) may be changed._

*)


(*** do-not-eval ***)
///Takes a seq<GFF<'a>>, a FASTA converter and a destination filepath and writes it into a .gff. Hint: Use converter = id if no FastA sequence is included.
let gffExampleWrite = GFF3.write BioItem.symbol (__SOURCE_DIRECTORY__ + "/data/gffExampleWrite.gff") features

(**
## Example: Sequence of CDS

If a FastA file is included you can look up the sequence of a CDS feature using the following function.

*)

(***do-not-eval***)
let firstCDS = 
    //get GFFEntries
    let filteredGFFEntries = 
        features 
        |> Seq.choose (fun x ->    
            match x with
            | GFF3.GFFEntryLine x -> Some x
            | _ -> None)

    //get all CDS features
    let filteredCDSFeatures =
        filteredGFFEntries
        |> Seq.filter (fun x -> x.Feature = "CDS")

    filteredCDSFeatures |> Seq.head

(***do-not-eval***)
let firstCDSSequence = GFF3.getSequence firstCDS features

//Output: Nucleotides.Nucleotides [] (ATG...TAA)

(**## Test?*)

open Plotly.NET

Chart.Point([1,2])
|> GenericChart.toChartHTML
(***include-it-raw***)