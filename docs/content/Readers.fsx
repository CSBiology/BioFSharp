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

(**
<br>
<a name="GenBank"></a>
GenBank
-------
<br>
<a id="SourceCode" href="https://github.com/CSBiology/BioFSharp/blob/master/src/BioFSharp.IO/GenBank.fs">&lt;/&gt;view source code</a>
<a id="APILink" href= >&#128194;View module documentation</a>
<a id="Author" href="https://github.com/kMutagene">&#128366;view author of this tutorial</a>
<br><br>
GenBank is the NIH genetic sequence database, an annotated collection of all publicly available DNA sequences
([Nucleic Acids Research, 2013 Jan;41(D1):D36-42](https://www.ncbi.nlm.nih.gov/pubmed/23193287)). 
A GenBank file contains various information about a sequence and its features.

The file structure of a GenBank (.gb) flat file can be split into 4 subsections:
<br>
<div id="responsiveTable">

| Section    | Description                                                                                                            | 
| ---------- | ---------------------------------------------------------------------------------------------------------------------- | 
| Meta       | Contains meta information about the annotated sequence, the file itself and the organism the sequence is found in      | 
| References | A collection of publications about the annotated sequence(or a subsequence of it) and information associated with them | 
| Features   | A collection of features and their position within the sequence                                                        | 
| Origin     | The annotated sequence itself                                                                                          | 

</div>
<br>
With the origins section being optional and the features section usually being by far the largest amongst them. 
For more information about the sections and their formatting, have a look at this 
[annotated sample record](https://www.ncbi.nlm.nih.gov/genbank/samplerecord/) over at NCBI. 

This file will also be used for the purpose of this tutorial and in plain text looks like this:

<br>
<button type="button" class="btn" data-toggle="collapse" data-target="#gbFileExample">Show/Hide example file</button>
<div id="gbFileExample" class="collapse fileExamples ">
    <pre>
        LOCUS       SCU49845                5028 bp    DNA     linear   PLN 14-JUL-2016 
        DEFINITION  Saccharomyces cerevisiae TCP1-beta gene, partial cds; and Axl2p     
                    (AXL2) and Rev7p (REV7) genes, complete cds.                        
        ACCESSION   U49845                                                              
        VERSION     U49845.1                                                            
        KEYWORDS    .                                                                   
        SOURCE      Saccharomyces cerevisiae (baker's yeast)                            
          ORGANISM  Saccharomyces cerevisiae                                            
                    Eukaryota; Fungi; Dikarya; Ascomycota; Saccharomycotina;            
                    Saccharomycetes; Saccharomycetales; Saccharomycetaceae;             
                    Saccharomyces.                                                      
        REFERENCE   1  (bases 1 to 5028)                                                
          AUTHORS   Roemer,T., Madden,K., Chang,J. and Snyder,M.                        
          TITLE     Selection of axial growth sites in yeast requires Axl2p, a novel    
                    plasma membrane glycoprotein                                        
          JOURNAL   Genes Dev. 10 (7), 777-793 (1996)                                   
           PUBMED   8846915                                                             
        REFERENCE   2  (bases 1 to 5028)                                                
          AUTHORS   Roemer,T.                                                           
          TITLE     Direct Submission                                                   
          JOURNAL   Submitted (22-FEB-1996) Biology, Yale University, New Haven, CT     
                    06520, USA                                                          
        FEATURES             Location/Qualifiers                                        
             source          1..5028                                                    
                             /organism="Saccharomyces cerevisiae"                       
                             /mol_type="genomic DNA"                                    
                             /db_xref="taxon:4932"                                      
                             /chromosome="IX"                                           
             mRNA            <1..>206                                                   
                             /product="TCP1-beta"                                       
             CDS             <1..206                                                    
                             /codon_start=3                                             
                             /product="TCP1-beta"                                       
                             /protein_id="AAA98665.1"                                   
                             /translation="SSIYNGISTSGLDLNNGTIADMRQLGIVESYKLKRAVVSSASEA 
                             AEVLLRVDNIIRARPRTANRQHM"                                   
             gene            <687..>3158                                                
                             /gene="AXL2"                                               
             mRNA            <687..>3158                                                
                             /gene="AXL2"                                               
                             /product="Axl2p"                                           
             CDS             687..3158                                                  
                             /gene="AXL2"                                               
                             /note="plasma membrane glycoprotein"                       
                             /codon_start=1                                             
                             /product="Axl2p"                                           
                             /protein_id="AAA98666.1"                                   
                             /translation="MTQLQISLLLTATISLLHLVVATPYEAYPIGKQYPPVARVNESF 
                             TFQISNDTYKSSVDKTAQITYNCFDLPSWLSFDSSSRTFSGEPSSDLLSDANTTLYFN 
                             VILEGTDSADSTSLNNTYQFVVTNRPSISLSSDFNLLALLKNYGYTNGKNALKLDPNE 
                             VFNVTFDRSMFTNEESIVSYYGRSQLYNAPLPNWLFFDSGELKFTGTAPVINSAIAPE 
                             TSYSFVIIATDIEGFSAVEVEFELVIGAHQLTTSIQNSLIINVTDTGNVSYDLPLNYV 
                             YLDDDPISSDKLGSINLLDAPDWVALDNATISGSVPDELLGKNSNPANFSVSIYDTYG 
                             DVIYFNFEVVSTTDLFAISSLPNINATRGEWFSYYFLPSQFTDYVNTNVSLEFTNSSQ 
                             DHDWVKFQSSNLTLAGEVPKNFDKLSLGLKANQGSQSQELYFNIIGMDSKITHSNHSA 
                             NATSTRSSHHSTSTSSYTSSTYTAKISSTSAAATSSAPAALPAANKTSSHNKKAVAIA 
                             CGVAIPLGVILVALICFLIFWRRRRENPDDENLPHAISGPDLNNPANKPNQENATPLN 
                             NPFDDDASSYDDTSIARRLAALNTLKLDNHSATESDISSVDEKRDSLSGMNTYNDQFQ 
                             SQSKEELLAKPPVQPPESPFFDPQNRSSSVYMDSEPAVNKSWRYTGNLSPVSDIVRDS 
                             YGSQKTVDTEKLFDLEAPEKEKRTSRDVTMSSLDPWNSNISPSPVRKSVTPSPYNVTK 
                             HRNRHLQNIQDSQSGKNGITPTTMSTSSSDDFVPVKDGENFCWVHSMEPDRRPSKKRL 
                             VDFSNKSNVNVGQVKDIHGRIPEML"                                 
             gene            complement(<3300..>4037)                                   
                             /gene="REV7"                                               
             mRNA            complement(<3300..>4037)                                   
                             /gene="REV7"                                               
                             /product="Rev7p"                                           
             CDS             complement(3300..4037)                                     
                             /gene="REV7"                                               
                             /codon_start=1                                             
                             /product="Rev7p"                                           
                             /protein_id="AAA98667.1"                                   
                             /translation="MNRWVEKWLRVYLKCYINLILFYRNVYPPQSFDYTTYQSFNLPQ 
                             FVPINRHPALIDYIEELILDVLSKLTHVYRFSICIINKKNDLCIEKYVLDFSELQHVD 
                             KDDQIITETEVFDEFRSSLNSLIMHLEKLPKVNDDTITFEAVINAIELELGHKLDRNR 
                             RVDSLEEKAEIERDSNWVKCQEDENLPDNNGFQPPKIKLTSLVGSDVGPLIIHQFSEK 
                             LISGDDKILNGVYSQYEEGESIFGSLF"                               
        ORIGIN                                                                          
                1 gatcctccat atacaacggt atctccacct caggtttaga tctcaacaac ggaaccattg     
               61 ccgacatgag acagttaggt atcgtcgaga gttacaagct aaaacgagca gtagtcagct     
              121 ctgcatctga agccgctgaa gttctactaa gggtggataa catcatccgt gcaagaccaa     
              181 gaaccgccaa tagacaacat atgtaacata tttaggatat acctcgaaaa taataaaccg     
              241 ccacactgtc attattataa ttagaaacag aacgcaaaaa ttatccacta tataattcaa     
              301 agacgcgaaa aaaaaagaac aacgcgtcat agaacttttg gcaattcgcg tcacaaataa     
              361 attttggcaa cttatgtttc ctcttcgagc agtactcgag ccctgtctca agaatgtaat     
              421 aatacccatc gtaggtatgg ttaaagatag catctccaca acctcaaagc tccttgccga     
              481 gagtcgccct cctttgtcga gtaattttca cttttcatat gagaacttat tttcttattc     
              541 tttactctca catcctgtag tgattgacac tgcaacagcc accatcacta gaagaacaga     
              601 acaattactt aatagaaaaa ttatatcttc ctcgaaacga tttcctgctt ccaacatcta     
              661 cgtatatcaa gaagcattca cttaccatga cacagcttca gatttcatta ttgctgacag     
              721 ctactatatc actactccat ctagtagtgg ccacgcccta tgaggcatat cctatcggaa     
              781 aacaataccc cccagtggca agagtcaatg aatcgtttac atttcaaatt tccaatgata     
              841 cctataaatc gtctgtagac aagacagctc aaataacata caattgcttc gacttaccga     
              901 gctggctttc gtttgactct agttctagaa cgttctcagg tgaaccttct tctgacttac     
              961 tatctgatgc gaacaccacg ttgtatttca atgtaatact cgagggtacg gactctgccg     
             1021 acagcacgtc tttgaacaat acataccaat ttgttgttac aaaccgtcca tccatctcgc     
             1081 tatcgtcaga tttcaatcta ttggcgttgt taaaaaacta tggttatact aacggcaaaa     
             1141 acgctctgaa actagatcct aatgaagtct tcaacgtgac ttttgaccgt tcaatgttca     
             1201 ctaacgaaga atccattgtg tcgtattacg gacgttctca gttgtataat gcgccgttac     
             1261 ccaattggct gttcttcgat tctggcgagt tgaagtttac tgggacggca ccggtgataa     
             1321 actcggcgat tgctccagaa acaagctaca gttttgtcat catcgctaca gacattgaag     
             1381 gattttctgc cgttgaggta gaattcgaat tagtcatcgg ggctcaccag ttaactacct     
             1441 ctattcaaaa tagtttgata atcaacgtta ctgacacagg taacgtttca tatgacttac     
             1501 ctctaaacta tgtttatctc gatgacgatc ctatttcttc tgataaattg ggttctataa     
             1561 acttattgga tgctccagac tgggtggcat tagataatgc taccatttcc gggtctgtcc     
             1621 cagatgaatt actcggtaag aactccaatc ctgccaattt ttctgtgtcc atttatgata     
             1681 cttatggtga tgtgatttat ttcaacttcg aagttgtctc cacaacggat ttgtttgcca     
             1741 ttagttctct tcccaatatt aacgctacaa ggggtgaatg gttctcctac tattttttgc     
             1801 cttctcagtt tacagactac gtgaatacaa acgtttcatt agagtttact aattcaagcc     
             1861 aagaccatga ctgggtgaaa ttccaatcat ctaatttaac attagctgga gaagtgccca     
             1921 agaatttcga caagctttca ttaggtttga aagcgaacca aggttcacaa tctcaagagc     
             1981 tatattttaa catcattggc atggattcaa agataactca ctcaaaccac agtgcgaatg     
             2041 caacgtccac aagaagttct caccactcca cctcaacaag ttcttacaca tcttctactt     
             2101 acactgcaaa aatttcttct acctccgctg ctgctacttc ttctgctcca gcagcgctgc     
             2161 cagcagccaa taaaacttca tctcacaata aaaaagcagt agcaattgcg tgcggtgttg     
             2221 ctatcccatt aggcgttatc ctagtagctc tcatttgctt cctaatattc tggagacgca     
             2281 gaagggaaaa tccagacgat gaaaacttac cgcatgctat tagtggacct gatttgaata     
             2341 atcctgcaaa taaaccaaat caagaaaacg ctacaccttt gaacaacccc tttgatgatg     
             2401 atgcttcctc gtacgatgat acttcaatag caagaagatt ggctgctttg aacactttga     
             2461 aattggataa ccactctgcc actgaatctg atatttccag cgtggatgaa aagagagatt     
             2521 ctctatcagg tatgaataca tacaatgatc agttccaatc ccaaagtaaa gaagaattat     
             2581 tagcaaaacc cccagtacag cctccagaga gcccgttctt tgacccacag aataggtctt     
             2641 cttctgtgta tatggatagt gaaccagcag taaataaatc ctggcgatat actggcaacc     
             2701 tgtcaccagt ctctgatatt gtcagagaca gttacggatc acaaaaaact gttgatacag     
             2761 aaaaactttt cgatttagaa gcaccagaga aggaaaaacg tacgtcaagg gatgtcacta     
             2821 tgtcttcact ggacccttgg aacagcaata ttagcccttc tcccgtaaga aaatcagtaa     
             2881 caccatcacc atataacgta acgaagcatc gtaaccgcca cttacaaaat attcaagact     
             2941 ctcaaagcgg taaaaacgga atcactccca caacaatgtc aacttcatct tctgacgatt     
             3001 ttgttccggt taaagatggt gaaaattttt gctgggtcca tagcatggaa ccagacagaa     
             3061 gaccaagtaa gaaaaggtta gtagattttt caaataagag taatgtcaat gttggtcaag     
             3121 ttaaggacat tcacggacgc atcccagaaa tgctgtgatt atacgcaacg atattttgct     
             3181 taattttatt ttcctgtttt attttttatt agtggtttac agatacccta tattttattt     
             3241 agtttttata cttagagaca tttaatttta attccattct tcaaatttca tttttgcact     
             3301 taaaacaaag atccaaaaat gctctcgccc tcttcatatt gagaatacac tccattcaaa     
             3361 attttgtcgt caccgctgat taatttttca ctaaactgat gaataatcaa aggccccacg     
             3421 tcagaaccga ctaaagaagt gagttttatt ttaggaggtt gaaaaccatt attgtctggt     
             3481 aaattttcat cttcttgaca tttaacccag tttgaatccc tttcaatttc tgctttttcc     
             3541 tccaaactat cgaccctcct gtttctgtcc aacttatgtc ctagttccaa ttcgatcgca     
             3601 ttaataactg cttcaaatgt tattgtgtca tcgttgactt taggtaattt ctccaaatgc     
             3661 ataatcaaac tatttaagga agatcggaat tcgtcgaaca cttcagtttc cgtaatgatc     
             3721 tgatcgtctt tatccacatg ttgtaattca ctaaaatcta aaacgtattt ttcaatgcat     
             3781 aaatcgttct ttttattaat aatgcagatg gaaaatctgt aaacgtgcgt taatttagaa     
             3841 agaacatcca gtataagttc ttctatatag tcaattaaag caggatgcct attaatggga     
             3901 acgaactgcg gcaagttgaa tgactggtaa gtagtgtagt cgaatgactg aggtgggtat     
             3961 acatttctat aaaataaaat caaattaatg tagcatttta agtataccct cagccacttc     
             4021 tctacccatc tattcataaa gctgacgcaa cgattactat tttttttttc ttcttggatc     
             4081 tcagtcgtcg caaaaacgta taccttcttt ttccgacctt ttttttagct ttctggaaaa     
             4141 gtttatatta gttaaacagg gtctagtctt agtgtgaaag ctagtggttt cgattgactg     
             4201 atattaagaa agtggaaatt aaattagtag tgtagacgta tatgcatatg tatttctcgc     
             4261 ctgtttatgt ttctacgtac ttttgattta tagcaagggg aaaagaaata catactattt     
             4321 tttggtaaag gtgaaagcat aatgtaaaag ctagaataaa atggacgaaa taaagagagg     
             4381 cttagttcat cttttttcca aaaagcaccc aatgataata actaaaatga aaaggatttg     
             4441 ccatctgtca gcaacatcag ttgtgtgagc aataataaaa tcatcacctc cgttgccttt     
             4501 agcgcgtttg tcgtttgtat cttccgtaat tttagtctta tcaatgggaa tcataaattt     
             4561 tccaatgaat tagcaatttc gtccaattct ttttgagctt cttcatattt gctttggaat     
             4621 tcttcgcact tcttttccca ttcatctctt tcttcttcca aagcaacgat ccttctaccc     
             4681 atttgctcag agttcaaatc ggcctctttc agtttatcca ttgcttcctt cagtttggct     
             4741 tcactgtctt ctagctgttg ttctagatcc tggtttttct tggtgtagtt ctcattatta     
             4801 gatctcaagt tattggagtc ttcagccaat tgctttgtat cagacaattg actctctaac     
             4861 ttctccactt cactgtcgag ttgctcgttt ttagcggaca aagatttaat ctcgttttct     
             4921 ttttcagtgt tagattgctc taattctttg agctgttctc tcagctcctc atatttttct     
             4981 tgccatgact cagattctaa ttttaagcta ttcaatttct ctttgatc                  
        //                                                                              
    </pre>
</div>
<br>

Reading GenBank files
---------------------
The type equivalent for a GenBank file in BioFSharp is a dictionary, mapping `string` keys to the `GenBankItem` type.
More information about the type modelling can be found in our [API reference]().
*)
open BioFSharp.IO

///Path of the example file
let exampleFilePath = __SOURCE_DIRECTORY__ + @"\data\sequence.gb"

///Parsed Example File 
let parsedGBFile = GenBank.Read.fromFile exampleFilePath

(**
The GenBank module provides various helper functions for querying items on the created Dictionary:
*)
///All features contained in this GenBank file representation
let features = GenBank.getFeatures parsedGBFile 

///All features with the "CDS" tag
let cdsFeatures = GenBank.getFeaturesWithType "CDS" parsedGBFile

(**
The `getfeaturesWithBaseSpan` function returns all Features that span the input BaseSpan of the sequence contained in the 
file. However, due to the way the BaseSpans are modelled it is currently only possible to find features that have exactly the query BaseSpan, 
or contain the exact BaseSpan in a join.
Getting all features that fall within the input BaseSpan will be a goal for a future update of this parser. 

We first need to create the BaseSpan that we want to find.
For that, we create a `BaseSpanRange` union case containing the type of BaseSpan (modelled as the `BaseSpanInformation` type)
and the start and end value as an integer tuple:
*)
///we are searching for a complete base span
let queryBaseSpanType = GenBank.BaseSpanInformation.Complete

///spanning nucleotides 687-3158
let queryBaseSpan = GenBank.BaseSpanRange.Single (queryBaseSpanType,(687,3158))

///
let bsQueryResult = GenBank.getFeaturesWithBaseSpan queryBaseSpan true parsedGBFile


(**
Writing GenBank files
---------------------
*)



