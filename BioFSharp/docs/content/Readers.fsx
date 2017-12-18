(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "../../bin/BioFSharp.dll"
#r "../../bin/BioFSharp.IO.dll"
#r "../../bin/FSharp.Care.dll"
#r "../../bin/FSharp.Care.IO.dll"
open BioFSharp
open BioFSharp.IO
open FSharp.Care.IO

(**

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



