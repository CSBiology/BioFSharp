(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "../../bin/BioFSharp.dll"
#r "../../bin/BioFSharp.IO.dll"

(**
Using Bio Format Readers
==============================
BioFSharp contains a set of readers for different biology-associated file formats. This documentation aims to give an introduction for them.

<a name="FastQ"></a>
FastQ
------------------------------
This documentation is not yet here. Hopefully it will be soon =)

*)

(*** hide ***)
open System
open BioFSharp
open BioFSharp.IO

(**
This module allows to parse FASTQ format data with original 4-lines entries into this record type
*)

/// FastqItem record contains header, sequence, qualityheader, qualitysequence of one entry
type FastqItem<'a,'b> = {
    Header    : string;
    Sequence  : 'a;
    QualityHeader : string;
    QualitySequence : 'b;       
}

(**
To be able to use this parser you need to define two converter functions, 
one example for each you can also find in our module, but you also may need to write your own.

We can convert sequence string to predefined option type of Amino Acids, using converter function
from our library 'BioFSharp.BioItemsConverter.OptionConverter'
*)

/// get characters as sequence units
let converterToAA string =
    string
    |> String.map (BioFSharp.BioItemsConverter.OptionConverter.charToOptionAminoAcid)

(**
If you have following possible values for quality sequence:
'!""#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~',
with Sanger format, that can encode a Phred quality score from 0 to 93 using ASCII 33 to 126, 
then you can use our converting function:
*)

/// get Phred quality score
let qualityConvertFn string =
    string
    |> String.toCharArray
    |> Array.map (fun i -> i.GetHashCode()-33)

(**
And then you can easily use this module to read your FastQ file
*)

let yourFastqFile = (__SOURCE_DIRECTORY__ + "/data/FastQtest.fastq")

let FastQSequence = 
    FastQ.fromFile convertFn qualityConvertFn yourFastqFile

(**


<hr>


<a name="FastA"></a>
FastA
------------------------------
This documentation is not yet here. Hopefully it will be soon =)

*)


(**


<hr>


<a name="GFF3"></a>
Introducing GFF3Parser
----------------------

The GFF3Parser is a tool to validate, read and write **GFF3** _(Generic Feature Format Version 3)_-Files.

All examples are executed on a modified version of [saccharomyces_cerevisiae.gff](http://downloads.yeastgenome.org/curation/chromosomal_feature/saccharomyces_cerevisiae.gff).

Introducing Generic Feature Format Version 3
--------------------------------------------

In GFF3 files every line represents one genomic feature with nine tab-delimited fields, whereas unlimited key-value pairs can be stored in field 9. 
The nine fields are: 

|Field          |   Description             |
| ------------- |:-------------:            |
| 1             | seqID                     |
| 2             | source                    |
| 3             | type, method or feature   |
| 4             | startPos                  |
| 5             | endPos                    |
| 6             | score                     |
| 7             | strand                    |
| 8             | phase                     |
| 9             | attributes                |

<img src="@Root/img/GFF3.png" alt="GFF3" style="width:150px;margin:10px" />

Directives (marked with "##[...]") provide additional information like the gff-version which has to be the first line of each file ("##gff-version 3[...]"). 
Comment lines have to start with a single "#[...]". It is possible that sequences in FastA format are at the end of the file. This has to be announced by a "##FASTA" directive line.

For further information visit [GFF3-Specifications](https://github.com/The-Sequence-Ontology/Specifications/blob/master/gff3.md).

How to use GFF3Parser
---------------------

To read in a gff you have to insert a filepath and optional a FastA converter. For further information about FastA visit [API Reference - FastA](https://csbiology.github.io/BioFSharp/reference/biofsharp-io-fasta.html).
*)

open BioFSharp.IO

///path and name of the input file
let filepathGFF = (__SOURCE_DIRECTORY__ + "/data/gff3Example.gff")

///converter for FastA.FastaItem
let fastaConverter (x:seq<char>) = 
    id x

///To read in a gff file you have to insert a FastA converter and a source filepath. 
///If no FastA Sequence is included you directly can use GFF3Parser.Reader.GFF3readerWithoutFasta filepathGFF
let gffExampleRead = GFF3Parser.Reader.GFF3reader fastaConverter filepathGFF 



(**
How to use GFF3SanityCheck
--------------------------

The GFF3SanityCheck prints wheter your GFF3 file is valid or not. If not, then it also returns the specified error and the line in which the error occured.
In contrast to GFF2 the field 3 (type) of a GFF3 entry is restricted to terms defined by the sequence ontology (SO) so this validator checks if the entry is a valid SO term.
You can find new versions of the SO at (https://sourceforge.net/projects/song/files/SO_Feature_Annotation).
*)

///to validate the gff file without SOTerm verification use this function and only insert filepath
let gffExampleSanityCheckSimple = GFF3Parser.SanityCheck.sanityCheck filepathGFF

///path, name and version of the 'Sequence Ontology terms'-file
let filepathSO_Terms = (__SOURCE_DIRECTORY__ + "/data/Sequence_Ontology_Terms_2_5_3.txt")

///to validate the gff file insert filepath
let gffExampleSanityCheck = GFF3Parser.SanityCheck.sanityCheckWithSOTerm filepathSO_Terms filepathGFF


(**
How to use GFF3RelationshipSearch
---------------------------------

You also can do a simple search for "_Parent_ - _child of_" relationships giving back all genomic features which contain the _searchterm_ in **ID/Id** or **Parent** field.

*)

///Term to search for:
let searchterm = "YAL069W"

///with this function you can search for a searchterm and you will get features which are related
let gffExampleSearch = GFF3Parser.Relationship.relationshipSearch gffExampleRead searchterm


(**
How to use GFF3Writer
---------------------

This writer takes GFF lines in stacks of 20,000 items and write/append them to a given filepath. 
If FastA sequences are included they are appended by a FastA writer described in the [API Reference - FastA](https://csbiology.github.io/BioFSharp/reference/biofsharp-io-fasta.html).

_Note: The order of key value pairs in field 9 (attributes) may be changed._

*)


///Takes a seq<GFF<'a>>, a FastA converter and a destination filepath and writes it into a .gff.
let gffExampleWrite = GFF3Parser.Writer.GFF3Writer gffExampleRead id (__SOURCE_DIRECTORY__ + "/data/gffExampleWrite.gff")



