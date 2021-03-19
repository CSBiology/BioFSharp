(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I @"../../bin/BioFSharp/net47/"
#I @"../../bin/BioFSharp.BioDB/net47/"
#I @"../../bin/BioFSharp.ImgP/net47"
#I @"../../bin/BioFSharp.IO/net47/"
#I @"../../bin/BioFSharp.Parallel/net47/"
#I @"../../bin/BioFSharp.Stats/net47/"
#I @"../../bin/BioFSharp.Vis/net47/"
#r @"../../packages/formatting/FSharp.Plotly/lib/netstandard2.0/FSharp.Plotly.dll"
#r "BioFSharp.IO"
#r "BioFSharp"
open BioFSharp
open BioFSharp.IO

(**
Pretty Printers
===============

BioFSharp is designed to work well with the FSI, as data evaluation usually involves a lot of scripting. The `BioFSharp.IO.FSIPrinters` module offers 
a bunch of functions to view our data types in a structured string format. This helps avoiding to save interim results for the purpose of 
visual investigation. To use these printers, use the `fsi.AddPrinter` function to register the desired printer. This will override the default 
printing behaviour of the respective type in the FSI.

We also provide a `BioFSharp.IO.fsx` convenience script in our nuget packages, that registers all (except modification printers) printers to the FSI.
just `#load` the script and you get all the printing goodness.


However, if you want to selectively register FSI printers,here are the currently implemented ones:

BioItems
--------

There are two options when printing BioItems concerning modifications:

 - `prettyPrintBioItem` will not directly print modifications, but indicates them by printing the symbol of the modified item in lowercase. 
 This will not display all available information, but in return keep collections nicely formatted, as you can see later.

 - `prettyPrintBioItemWithModifications` will print all modifications on BioItems in square brackets appended to the modified items symbol
*)

let aa = AminoAcids.Ala
let nuc = Nucleotides.T

//a twofold modified Alanine
let modifiedAA =
    aa
    |> AminoAcids.setModification (ModificationInfo.Table.N15)
    |> AminoAcids.setModification (ModificationInfo.Table.CO)

(** 
Standard console output:
*)

(***include-value:aa***)
(***include-value:nuc***)
(***include-value:modifiedAA***)

(***hide***)
let aaPrint         = FSIPrinters.prettyPrintBioItem aa
let nPrint          = FSIPrinters.prettyPrintBioItem nuc
let aaPrintWithMod  = FSIPrinters.prettyPrintBioItem modifiedAA
let aaPrint'        = FSIPrinters.prettyPrintBioItemWithModifications aa
let nPrint'         = FSIPrinters.prettyPrintBioItemWithModifications nuc
let aaPrintWithMod' = FSIPrinters.prettyPrintBioItemWithModifications modifiedAA

(** 
Console output using `prettyPrintBioItem`:
*)

//register the desired printer
fsi.AddPrinter(FSIPrinters.prettyPrintBioItem)

(***include-value:aaPrint***)
(***include-value:nPrint***)
(***include-value:aaPrintWithMod***)

(** 
Console output using `prettyPrintBioItemWithModifications`:
*)

//register the desired printer
fsi.AddPrinter(FSIPrinters.prettyPrintBioItemWithModifications)

(***include-value:aaPrint'***)
(***include-value:nPrint'***)
(***include-value:aaPrintWithMod'***)

(**
BioCollections
--------------

The pretty printers for BioCollections (`BioSeq`,`BioArray`, and `BioList`) show the contents of the collections as
6 x 10 space-sepearated item blocks per line, preceeded by an index indicator. This is similar to the ORIGIN section of a genebank file.
*)

//an amino acid sequence
let largerSequence = 
    """MASSMLSSATMVASPAQATMVAPFNGLKSSAAFPATRKANNDITSITSNGGRVNCMQVWP
    PIGKKKFETLSYLPDLTDSELAKEVDYLIRNKWIPCVEFELEHGFVYREHGNSPGYYDGR
    YWTMWKLPLFGCTDSAQVLKEVEECKKEYPNAFIRIIGFDNTRQVQCISFIAYKPPSFT
    MASSMLSSATMVASPAQATMVAPFNGLKSSAAFPATRKANNDITSITSNGGRVNCMQVWP
    PIGKKKFETLSYLPDLTDSELAKEVDYLIRNKWIPCVEFELEHGFVYREHGNSPGYYDGR
    YWTMWKLPLFGCTDSAQVLKEVEECKKEYPNAFIRIIGFDNTRQVQCISFIAYKPPSFT
    MASSMLSSATMVASPAQATMVAPFNGLKSSAAFPATRKANNDITSITSNGGRVNCMQVWP
    PIGKKKFETLSYLPDLTDSELAKEVDYLIRNKWIPCVEFELEHGFVYREHGNSPGYYDGR
    YWTMWKLPLFGCTDSAQVLKEVEECKKEYPNAFIRIIGFDNTRQVQCISFIAYKPPSFT""" 
    |> BioArray.ofAminoAcidString

//the same amino acid sequence with a 15N labeled Alanine at position 4
let modifiedSequence =
    Array.copy largerSequence
modifiedSequence.[3] <- AminoAcids.setModification ModificationInfo.Table.N15 AminoAcids.Ala

(***hide***)
let colPrnt     = FSIPrinters.prettyPrintBioCollection largerSequence
let colPrntMod  = FSIPrinters.prettyPrintBioCollection modifiedSequence
let colPrnt'    = FSIPrinters.prettyPrintBioCollectionWithModifications largerSequence
let colPrntMod' = FSIPrinters.prettyPrintBioCollectionWithModifications modifiedSequence

(** 
Standard console output:
*)

(*** include-value:largerSequence ***)
(*** include-value:modifiedSequence ***)

(** 
Console output using `prettyPrintBioCollection`:
*)

//register the desired printer
fsi.AddPrinter(FSIPrinters.prettyPrintBioCollection)

(*** include-value:colPrnt ***)
(*** include-value:colPrntMod ***)


(** 
When printing modifications as shown before with BioItems, the blocks are not aligned anymore, but more information is displayed.

Console output using `prettyPrintBioCollectionWithModifications`:
*)

//register the desired printer
fsi.AddPrinter(FSIPrinters.prettyPrintBioCollectionWithModifications)

(*** include-value:colPrnt' ***)
(*** include-value:colPrntMod' ***)

(**
Clustal formatted alignments:
-----------------------------

The `prettyPrintClustal` printer displays clustal alignments just as they would apper in a .asn file:
*)

let path = __SOURCE_DIRECTORY__ + "/data/clustalExample.asn"

//Clustal algnment read in from a clustal formated .asn file
let clustalAlignment = Clustal.ofFile path

(** 
Standard console output:
*)

(*** include-value:clustalAlignment ***)

(***hide***)
let clustalPrnt = FSIPrinters.prettyPrintClustal clustalAlignment

(** 
Console output using `prettyPrintClustal`:
*)

//register the desired printer
fsi.AddPrinter(FSIPrinters.prettyPrintClustal)

(*** include-value:clustalPrnt ***)


(**
SOFT
----
There are 5 printers available:

`prettyPrintGSE` and `prettyPrintGPL` format the top level types `SOFT.Series.GSE` and `SOFT.Platform.GPL` including associated record metadata.

`prettyPrintSampleRecord`,`prettyPrintSeriesRecord`, and `prettyPrintPlatformRecord` format single record metadata.

All SOFT types are very large and hard to read from standard output, especially the nested top level types, so we will omitt the standard output for readability.
*)

//register the desired printers
fsi.AddPrinter(FSIPrinters.prettyPrintGPL)
fsi.AddPrinter(FSIPrinters.prettyPrintGSE)

fsi.AddPrinter(FSIPrinters.prettyPrintSampleRecord)
fsi.AddPrinter(FSIPrinters.prettyPrintSeriesRecord)
fsi.AddPrinter(FSIPrinters.prettyPrintPlatformRecord)

let gplPath = __SOURCE_DIRECTORY__ + "/data/GPL15922_family.soft"

let gpl15922 = SOFT.Platform.fromFile gplPath

(***hide***)
let gplPrnt = FSIPrinters.prettyPrintGPL gpl15922

(** 
Console output using `prettyPrintGPL`:
*)

(*** include-value:gplPrnt ***)

let gsePath = __SOURCE_DIRECTORY__ + "/data/GSE71469_family.soft"

let gse71469 = SOFT.Series.fromFile gsePath

(***hide***)
let gsePrnt = FSIPrinters.prettyPrintGSE gse71469

(** 
Console output using `prettyPrintGSE`:
*)

(*** include-value:gsePrnt ***)

let smplRecord = 
    gse71469
    |> SOFT.Series.getAssociatedSamples
    |> List.item 0

(***hide***)
let smplPrint = FSIPrinters.prettyPrintSampleRecord smplRecord

(** 
Console output using `prettyPrintSampleRecord`:
*)

(*** include-value:smplPrint ***)

let seriesRecord = 
    gpl15922
    |> SOFT.Platform.getAssociatedSeries
    |> List.item 0

(***hide***)
let seriesPrint = FSIPrinters.prettyPrintSeriesRecord seriesRecord

(** 
Console output using `prettyPrintSeriesRecord`:
*)

(*** include-value:seriesPrint ***)


let pltfrmRecord = 
    gse71469
    |> SOFT.Series.getAssociatedPlatforms
    |> List.item 0

(***hide***)
let pltfrmPrint = FSIPrinters.prettyPrintPlatformRecord pltfrmRecord

(** 
Console output using `prettyPrintSampleRecord`:
*)

(*** include-value:pltfrmPrint ***)
