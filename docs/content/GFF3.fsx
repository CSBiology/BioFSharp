(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "../../bin/BioFSharp.dll"
#r "../../bin/BioFSharp.IO.dll"
#r "../../bin/FSharp.Care.dll"
#r "../../bin/FSharp.Care.IO.dll"
open BioFSharp



(**
#Introducing GFF3Parser

<br>
<a id="SourceCode" href="https://github.com/CSBiology/BioFSharp/blob/master/src/BioFSharp.IO/GFF3Parser.fs">&lt;/&gt;view source code</a>
<a id="Author" href="https://github.com/bvenn">&#128366;view author of this tutorial</a>
<br><br>
The GFF3Parser is a tool to validate, read and write **GFF3** _(Generic Feature Format Version 3)_-Files.

All examples are executed on a modified version of [saccharomyces_cerevisiae.gff](http://downloads.yeastgenome.org/curation/chromosomal_feature/saccharomyces_cerevisiae.gff).

Introducing Generic Feature Format Version 3
--------------------------------------------

In GFF3 files every line represents one genomic feature with nine tab-delimited fields, whereas unlimited key-value pairs can be stored in field 9. 
GFFEntries are implemented as a record type containing the following fields:
<br><br>

<div id="responsiveTable">

| Field name | type                      | Description                                                                                                                                               | Example                                                          |
| ---------- | ------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------- |
| Seqid      | `string`                  | ID of the landmark                                                                                                                                        | "chrI"                                                           |
| Source     | `string`                  | The source is a free text qualifier to describe the algorithm or operating procedure that generated this feature                                          | "SGD"                                                            |
| Feature    | `string`                  | also called method or type; should be a term from the Sequence Ontology or an SO accession number                                                         | "gene"                                                           |
| StartPos   | `int`                     | start coordinate of the feature (1-based interger coordinate)                                                                                             | 335                                                              |
| EndPos     | `int`                     | end coordinate of the feature (1-based interger coordinate)                                                                                               | 649                                                              |
| Score      | `float`                   | The score is a not well defined floating point number representing different values depending of what you are looking at (nan  if no phase is avaliable)  | nan                                                              |
| Strand     | `char`                    | strand of the feature ('\000'  if no phase is avaliable)                                                                                                  | '+'                                                              |
| Phase      | `int`                     | for features of type CDS; indicates where the feature begins with reference to the reading frame (-1 if no phase is avaliable)                            | -1                                                               |
| Attributes | `Map<string,string list>` | The field Attributes can contain multiple tag-value pairs, some with predefined meanings. Please check the specification website for further information. | seq [[ID, [YAL069W]];[Ontology_term, [GO:0000004; GO:0005554]]]  |
| Supplement | `string []`               | If additional columns (e.g. counts) are present, they are written in this tab-delimited string array.                                                     | [|""|]                                                           |

</div>
<br>
Below you can see an example GFF3 file.

<img src="@Root/img/GFF3.png" alt="GFF3" style="width:150px;margin:10px" />
<br>
<div id="responsiveTable">

| Line number| Description                 | 
| ---------- | --------------------------- | 
| 1          | Declaration of file version | 
| 2          | Comment                     | 
| 4+5        | GFF entry lines             | 
| 6          | FastA directive             | 
| 7+         | Fasta Sequence              | 

</div>
<br>

Directives (marked with "##[...]") provide additional information like the gff-version which has to be the first line of each file ("##gff-version 3[...]"). 
Comment lines have to start with a single "#[...]". It is possible that sequences in FastA format are at the end of the file. This has to be announced by a "##FASTA" directive line.

For further information visit [GFF3-Specifications](https://github.com/The-Sequence-Ontology/Specifications/blob/master/gff3.md).

How to use GFF3Parser
---------------------

To read in a gff you have to insert a filepath and optional a FASTA converter. For further information about FASTA check the [FASTA section](https://csbiology.github.io/BioFSharp/Readers.html#FastA) on this site 
or visit [API Reference - FastA](https://csbiology.github.io/BioFSharp/reference/biofsharp-io-fasta.html).
*)

open BioFSharp.IO

///path and name of the input file
let filepathGFF = (__SOURCE_DIRECTORY__ + "/data/gff3Example.gff")

///To read in a gff file you have to insert a FASTA converter and a source filepath. 
///If no FASTA Sequence is included you directly can use GFF3Parser.Reader.GFF3readerWithoutFasta [filepathGFF].
let gffExampleRead = GFF3Parser.GFF3reader BioFSharp.BioArray.ofNucleotideString filepathGFF 



(**
How to use GFF3SanityCheck
--------------------------

The GFF3SanityCheck prints wheter your GFF3 file is valid or not. It returns all specified errors including the lines in which the errors occured.
In contrast to GFF2 the field 3 (type, feature or method) of a GFF3 entry is restricted to terms defined by the sequence ontology (SO) so this validator checks if the entry is a valid SO term.
You can find new versions of the SO at (https://sourceforge.net/projects/song/files/SO_Feature_Annotation).
*)

///to validate the gff file without SOTerm verification use this function and only insert filepath
let gffExampleSanityCheckSimple = GFF3Parser.sanityCheck filepathGFF

///path, name and version of the 'Sequence Ontology terms'-file
let filepathSO_Terms = (__SOURCE_DIRECTORY__ + "/data/Sequence_Ontology_Terms_2_5_3.txt")

///to validate the gff file insert filepath
let gffExampleSanityCheck = GFF3Parser.sanityCheckWithSOTerm filepathSO_Terms filepathGFF


(**
How to use GFF3RelationshipSearch
---------------------------------

You also can do a simple search for "_Parent_ - _child of_" relationships giving back all genomic features which contain the _searchterm_ in **ID/Id** or **Parent** field.

*)

///Term to search for:
let searchterm = "YAL069W"

///with this function you can search for a searchterm and you will get features which are related
let gffExampleSearch = GFF3Parser.relationshipSearch gffExampleRead searchterm


(**
How to use GFF3Writer
---------------------

This writer takes GFF lines in stacks of 20,000 items and write/append them to a given filepath. 
If FastA sequences are included they are appended by a FastA writer described in the [API Reference - FastA](https://csbiology.github.io/BioFSharp/reference/biofsharp-io-fasta.html).

_Note: The order of key value pairs in field 9 (attributes) may be changed._

*)


///Takes a seq<GFF<'a>>, a FASTA converter and a destination filepath and writes it into a .gff.
let gffExampleWrite = GFF3Parser.GFF3Writer gffExampleRead BioItem.symbol (__SOURCE_DIRECTORY__ + "/data/gffExampleWrite.gff")
