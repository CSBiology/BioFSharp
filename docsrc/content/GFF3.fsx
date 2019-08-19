(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I @"../../bin/BioFSharp/net47/"
#I @"../../bin/BioFSharp.BioDB/net45/"
#I @"../../bin/BioFSharp.ImgP/net47"
#I @"../../bin/BioFSharp.IO/net47/"
#I @"../../bin/BioFSharp.Parallel/net47/"
#I @"../../bin/BioFSharp.Stats/net47/"
#I @"../../bin/BioFSharp.Vis/net47/"
#r @"../../lib/Formatting/FSharp.Plotly.dll"
#r "FSharpAux.dll"
#r "FSharpAux.IO.dll"
#r "BioFSharp.dll"
#r "BioFSharp.IO.dll"
open BioFSharp

open FSharpAux

(**
<table class="HeadAPI">
<td class="Head"><h1>Generic Feature Format Version 3</h1></td>
<td class="API">
    <a id="APILink" href="https://csbiology.github.io/BioFSharp/reference/biofsharp-gff3.html" >&#128194;View module documentation</a>
</td>
</table>
In GFF3 files every line represents one genomic feature with nine tab-delimited fields, whereas unlimited key-value pairs can be stored in field 9. 
It is possible to link multiple features to genomic units using the 'Parent tag'.

In the following you can see a GFF file example (modified version of [saccharomyces_cerevisiae.gff](http://downloads.yeastgenome.org/curation/chromosomal_feature/saccharomyces_cerevisiae.gff)):

![GFF3](img/GFF3.png)

Directives (marked with "##[...]") provide additional information like the gff-version which has to be the first line of each file ("##gff-version 3[...]"). 
Comment lines have to start with a single "#[...]". It is possible that sequences in FastA format are attached at the end of the file. This has to be announced by a "##FASTA" directive line.

For further information visit [GFF3-Specifications](https://github.com/The-Sequence-Ontology/Specifications/blob/master/gff3.md).

##Reading GFF3 files


To read in a gff you have to insert a filepath and optionally a FastA converter. For further information about FastA check the [FastA section](https://csbiology.github.io/BioFSharp/FastA.html)
or visit [API Reference - FastA](https://csbiology.github.io/BioFSharp/reference/biofsharp-io-fasta.html).
*)

open BioFSharp.IO

//path of the input file
let filepathGFF = (__SOURCE_DIRECTORY__ + "/data/gff3Example.gff")

//reads from file to seq of GFFLines
//If no FASTA Sequence is included you directly can use GFF3.fromFileWithoutFasta [filepathGFF].
let features = GFF3.fromFile BioFSharp.BioArray.ofNucleotideString filepathGFF 



(**
###How to use GFF3SanityCheck


The GFF3SanityCheck prints wether your GFF3 file is valid or not. It returns all specified errors including the lines in which they occured.
In contrast to GFF2 the field 3 (type, feature or method) of a GFF3 entry is restricted to terms defined by the sequence ontology (SO) so this validator is able to check if the entry is a valid SO term.
You can find new versions of the SO at (https://sourceforge.net/projects/song/files/SO_Feature_Annotation).
*)

//to validate the GFF file without SOTerm verification use this function and only insert the filepath
let featuresSanityCheck = GFF3.sanityCheck filepathGFF

//path, name and version of the 'Sequence Ontology terms'-file
let filepathSO_Terms = (__SOURCE_DIRECTORY__ + "/data/Sequence_Ontology_Terms_2_5_3.txt")

//to validate the gff file insert filepath
let featuresSanityCheckWithSOTerm = GFF3.sanityCheckWithSOTerm filepathSO_Terms filepathGFF


(**
###How to use GFF3RelationshipSearch


You also can do a simple search for "Parent - child of" relationships giving back all genomic features which contain the searchterm in **ID/Id** or **Parent** field.

*)

///Term to search for:
let searchterm = "YAL069W"

///with this function you can search features which are related to the searchterm
let gffExampleSearch = GFF3.relationshipSearch features searchterm


(**
##Writing GFF3 files

In order to write a sequence of (GFFLine<_>) into a file use the following function.
If FastA sequences are included they are appended by a FastA writer described in the [API Reference - FastA](https://csbiology.github.io/BioFSharp/reference/biofsharp-io-fasta.html).

_Note: The order of key value pairs in field 9 (attributes) may be changed._

*)


///Takes a seq<GFF<'a>>, a FASTA converter and a destination filepath and writes it into a .gff. Hint: Use converter = id if no FastA sequence is included.
(*** do-not-eval ***)
let gffExampleWrite = GFF3.write BioItem.symbol (__SOURCE_DIRECTORY__ + "/data/gffExampleWrite.gff") features

(**
##Example: Sequence of CDS

If a FastA file is included you can look up the sequence of a CDS feature using the following function.

*)

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


let firstCDSSequence = GFF3.getSequence firstCDS features

//Output: Nucleotides.Nucleotides [] (ATG...TAA)