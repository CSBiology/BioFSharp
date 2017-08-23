(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "../../bin/BioFSharp.dll"
#r "../../bin/BioFSharp.IO.dll"
#r "../../bin/FSharp.Care.dll"
#r "../../bin/FSharp.Care.IO.dll"

(**
Using Bio Format Readers
==============================
BioFSharp contains a set of readers for different biology-associated file formats. This documentation aims to give an introduction for them.

<a name="FastQ"></a>
FastQ
------------------------------
<br>
<a id="SourceCode" href="https://github.com/CSBiology/BioFSharp/blob/master/src/BioFSharp.IO/FastQ.fs">&lt;/&gt;view source code</a>
<a id="Author" href="https://github.com/MikhayN">&#128366;view author of this tutorial</a>
<br><br>
*)

(*** hide ***)
open System
open BioFSharp
open BioFSharp.IO
open FSharp.Care
open FSharp.Care.IO

(**
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

We can convert sequence string to predefined option type of Amino Acids, using converter function
from our library 'BioFSharp.BioItemsConverter.OptionConverter'
*)

/// get characters as sequence units
let converterToAA string =
    string
    |> String.toCharArray
    |> Array.map (BioFSharp.BioItemsConverter.OptionConverter.charToOptionAminoAcid)

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
    FastQ.fromFile converterToAA qualityConvertFn yourFastqFile

(**


<hr>


<a name="FastA"></a>
Introducing FASTA format
------------------------
<br>
<a id="SourceCode" href="https://github.com/CSBiology/BioFSharp/blob/master/src/BioFSharp.IO/FastA.fs">&lt;/&gt;view source code</a>
<a id="Author" href="https://github.com/bvenn">&#128366;view author of this tutorial</a>
<br><br>

The FASTA format can be used to represent sequences of amino acids or nucleotides written in single-letter code 
(for further description of single-letter codes visit [Nucleotides](https://csbiology.github.io/BioFSharp/reference/biofsharp-nucleotides-nucleotide.html) or [AminoAcids](https://csbiology.github.io/BioFSharp/reference/biofsharp-aminoacids-aminoacid.html) respectively). 
One sequence constists of two parts: The first line (Header) starting with a ">" is followed by a sequence identification code which should represent a unique description of the sequence. 
Subsequent lines contain the sequence itself, which is separated into chunks of 60 to 80 characters per line.
For further information about the format please visit [NCBI - FASTA](https://blast.ncbi.nlm.nih.gov/Blast.cgi?CMD=Web&PAGE_TYPE=BlastDocs&DOC_TYPE=BlastHelp).

Sequences in FASTA format are implemented as a record type containing the following fields:
<br><br>

<div id="responsiveTable">

| Field name    | type      | Description                                               | Example                                                                |
| ------------- | --------- | --------------------------------------------------------- | ---------------------------------------------------------------------- |
| Header        | `string`  | sequence identification code                              | "gi&#124;0000&#124;gb&#124;accession&#124;0001&#124; example sequence" |
| Sequence      | `'a`      | Sequence of nucleotides or aminoacids in type of choice   | BioArray.BioArray<AminoAcids.AminoAcid>                                |


</div>
<br>>

Creating an FastaItem
---------------------

To create a FastaItem type you directly can use the provided `create` function.
<br>
*)

open BioFSharp.IO.FastA

//creates a FastaItem out of a header and a sequence in given type
let exampleFasta = FastA.createFastaItem "gi|0000|gb|accession|0001| example sequence" ([|'M';'F';'H';'F';'I';'L';'P';'F';'T'|] |> BioArray.ofAminoAcidString)
(**
<br>

Reading FastaItem
-----------------


You can convert the sequence of `chars` to predefined sequences of `AminoAcids` or `Nucleotides`, using converter function
from our library [BioFSharp.BioList](https://github.com/CSBiology/BioFSharp/blob/master/src/BioFSharp/BioList.fs), 
[BioFSharp.BioArray](https://github.com/CSBiology/BioFSharp/blob/master/src/BioFSharp/BioArray.fs) 
or [BioFSharp.BioSeq](https://github.com/CSBiology/BioFSharp/blob/master/src/BioFSharp/BioSeq.fs).

#### There are multiple options to read in a FASTA file:

1. You can read in the `.fasta` with the path of the file.
*)

let filepathFASTA = (__SOURCE_DIRECTORY__ + "/data/FASTAExample1.fasta")

//reads from file to an array of FastaItems.
let exampleFromFile = FastA.fromFile BioArray.ofAminoAcidString filepathFASTA

(**
<br>
2. If the FASTA file is compressed in a gzip file you can read it using the same parameters as above and the following function.

*)

let filepathGzipFASTA = (__SOURCE_DIRECTORY__ + "/data/FASTAExample2.gz")

//reads from gzip file to an List of FastaItems.
let exampleFromGzipFile  = FastA.fromGzipFile BioList.ofAminoAcidString filepathGzipFASTA


(**
<br>
3. To convert `seq<string>` to `seq<FastaItem>` you can use the following function.
*)

open FSharp.Care.IO

let stringSequence = FileIO.readFile(filepathFASTA)

//reads seq<string> to sequence of FastaItems
let exampleFromFileEnumerator = FastA.fromFileEnumerator BioSeq.ofAminoAcidString stringSequence


(**
<br>
To write a `seq<FastaItem>` into a file use the following function.
*)

let seqFastaItem = seq [exampleFasta]

//writes a sequence of FastaItems into a .fasta file
FastA.write BioItem.symbol (__SOURCE_DIRECTORY__ + "/data/FASTAExample3.fasta") seqFastaItem 


(**


<hr>


<a name="GFF3"></a>
Introducing GFF3Parser
----------------------
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
<br>>
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
<br>>

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



