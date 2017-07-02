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

let a = 42
printfn "hello world: %i " a



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

All examples are executed on [saccharomyces_cerevisiae.gff](http://downloads.yeastgenome.org/curation/chromosomal_feature/saccharomyces_cerevisiae.gff).

Introducing Generic Feature Format Version 3
--------------------------------------------

In GFF3 files every line represents one genomic feature with nine tab-delimited fields, whereas unlimited key-value pairs can be stored in field 9 (**attributes**). 
The nine fields are: [**seqID**] [**source**] [**type,feature or method**] [**start**] [**end**] [**score**] [**strand**] [**phase**] [**attributes**].

Directives (marked with **##**_[...]_ ) provide additional information like the gff-version which has to be the first line of each file (_"##gff-version 3[...]"_). 
Comments lines have to start with a single **#**_[...]_. It is possible to have FastA sequences at the end of the file. This has to be announced by a _"##FASTA"_ directive line.

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

///to read in gff file insert converter and filepath. 
///You can also directly use GFF3Parser.Reader.GFF3readerWithoutFasta filepathGFF
let gffExampleRead = GFF3Parser.Reader.GFF3reader id filepathGFF 



(**
How to use GFF3Validator
------------------------

The GFF3Validator prints wheter your GFF3 file is valid or not. If not, then it also returns the specified error and the line in which the error occured.
In contrast to GFF2 the field 3 (**type**) is restricted to terms defined by the sequence ontology (SO) so this validator checks if the entry is a valid SO_Term.
You can find new versions of the SO [here](https://sourceforge.net/projects/song/files/SO_Feature_Annotation).
*)

///path, name and version of the 'Sequence Ontology terms'-file
let filepathSO_Terms = (__SOURCE_DIRECTORY__ + "/data/Sequence_Ontology_Terms_2_5_3.txt")

///to validate the gff file insert filepath
let gffExampleValidate = GFF3Parser.Validator.GFF3validator filepathSO_Terms filepathGFF

(**
How to use GFF3RelationshipSearch
---------------------------------
You also can do a simple search for _Parent_ - _child_of_ relationships giving back all genomic features which contain the _searchterm_ in **ID** or **Parent** field.

*)

///Term to search for:
let searchterm = "YAL069W"

///with this function you can search for a searchterm and you will get features which are related
let gffExampleSearch = GFF3Parser.Relationship.relationshipSearch gffExampleRead searchterm


(**
How to use GFF3Writer
---------------------


*)

///coming soon

