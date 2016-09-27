(*** hide ***)
#r "../../bin/BioFSharp.dll"
#r "../../bin/BioFSharp.IO.dll"

(**
Introducing BlastWrapper
========================

BlastWrapper is a tool for performing different tasks and in NCBI BLAST console applications (version 2.2.31+).
It is able to create BLAST databases and perform blastN or blastP algorithms, while providing a way to set
any output parameter for creating a custom output format.
Official documentation for all BLAST applications can be found [here] (http://www.ncbi.nlm.nih.gov/books/NBK279690).

For the purpose of this tutorial, we will build a protein database using a `.fastA` file containing chloroplast proteins
of Chlamydomonas reinhardtii included in BioFSharp/docs/content/data. 

Our query protein for the subsequent BLAST search will be the [photosystem II protein D1][linkToFasta] from Arabidopsis thaliana chloroplast.
    [linkToFasta]: http://www.ncbi.nlm.nih.gov/protein/7525013?report=fasta

How to use BlastWrapper
=======================

1. Creation of a BLAST database
-------------------------------
We will use the minimal amount of parameters needed to create a BLAST database from an input file. 
The created database files will have the same name as the input file and will be located in the same folder. 
However, there are many parameters you can use to specify your database. Please refer to the [NCBI user manual][manual] for more information.
    [manual]: http://www.ncbi.nlm.nih.gov/books/NBK279675/ 

First, lets specify the path of our input and the type of our database. Use a string for the input path and the provided `MakeDbParams` type
for every other parameter.
*)

open BioFSharp
open BioFSharp.IO
open BlastNCBI
open Parameters

///path and name of the input file/output database. 
let inputFile = (__SOURCE_DIRECTORY__ + "/data/Chlamy_Cp.fastA")

///defines database type (in this case: a protein database)
let typeOfDatabase = Parameters.MakeDbParams.DbType Parameters.Protein

(**
The wrapper needs to know the path of the ncbi applications.
*)

///the path of the /bin folder where the BLAST applications are located
let ncbiPath = (__SOURCE_DIRECTORY__ + "/../../lib/ncbi-blast/bin")

(**
We now provide the wrapper our ncbi path, the input path and a sequence of parameters (containing just one parameter in this case, the database type).
*)

BlastWrapper(ncbiPath).makeblastdb inputFile ([typeOfDatabase;] |> seq<Parameters.MakeDbParams>)

(**
This creates 3 new files in our directory:
`Chlamy_Cp.fastA.phr`,`Chlamy_Cp.fastA.pin` and `Chlamy_Cp.fastA.psq`.
We have sucesssfully created our search database.


2. Creating a `.fastA` file from an aminoacid string
-------------------------------------------------------------
_Note: this step is not necessary if you want to use an already existing file as query. If this is the case, skip to step 3._

First, lets specify a string with our aminoacid sequence and convert it to a `BioSeq`.
For more information about `BioSeq`, please refer to this [documentation] (https://csbiology.github.io/BioFSharp/reference/biofsharp-bioseq.html)
*)

///Raw string of the aminoacid sequence of our query protein
let aminoacidString = "MTAILERRESESLWGRFCNWITSTENRLYIGWFGVLMIPTLLTATSVFIIAFIAAPPVDIDGIREPVSGS
LLYGNNIISGAIIPTSAAIGLHFYPIWEAASVDEWLYNGGPYELIVLHFLLGVACYMGREWELSFRLGMR
PWIAVAYSAPVAAATAVFLIYPIGQGSFSDGMPLGISGTFNFMIVFQAEHNILMHPFHMLGVAGVFGGSL
FSAMHGSLVTSSLIRETTENESANEGYRFGQEEETYNIVAAHGYFGRLIFQYASFNNSRSLHFFLAAWPV
VGIWFTALGISTMAFNLNGFNFNQSVVDSQGRVINTWADIINRANLGMEVMHERNAHNFPLDLAAVEAPS
TNG"
///header for the ``` file
let header = ">gi|7525013|ref|NP_051039.1| photosystem II protein D1 (chloroplast) [Arabidopsis thaliana]"

///Query sequency represented as a sequence of `AminoAcid` one of BioFSharp`s `BioItems`
let querySequence = BioSeq.ofAminoAcidString aminoacidString

(**
We will now use BioFSharp`s `FastA` library to create a `FASTA` item and write it to a file.
*)

///path and name of the query file
let queryFastaPath = __SOURCE_DIRECTORY__ + "/data/testQuery.fastA"

///FastaItem containing header string and query sequence
let queryFastaItem = FastA.createFastaItem header querySequence

(**
To create our `.fastA` file, we need to use the `BioItem.symbol` converter, which will convert the 3 letter code of the aminoacids in our biosequence
to the one letter symbol (eg. Met -> M)
*)

FastA.write BioItem.symbol queryFastaPath [queryFastaItem;] 

(**
3. Performing the BLAST search
------------------------------
We have created our search database and the query we want to find. Before we can perform the actual search, we need to define the BLAST prameters.
_Note: custom output formats can only be specified for output types `CSV`, `tabular` and `tabular with comments`. For more information, check 
the [options for the command-line applicaions](http://www.ncbi.nlm.nih.gov/books/NBK279675/)_

First, lets specify the overall output type. This will define the outline of our output. We want our output to be in tabular form, with added information
in the form of comments.
_Note: when not specified, the output type will be `pairwise`_
*)

///overall outline of the output 
let outputType = OutputType.TabularWithComments

(**
We have a large selection of parameters that we can include in the output.  
*)

///a sequence of custom output format parameters
let outputFormat= 
    
    [   BlastNCBI.Parameters.OutputCustom.Query_SeqId; BlastNCBI.Parameters.OutputCustom.Subject_SeqId;BlastNCBI.Parameters.OutputCustom.Query_Length;
        BlastNCBI.Parameters.OutputCustom.Subject_Length;BlastNCBI.Parameters.OutputCustom.AlignmentLength;BlastNCBI.Parameters.OutputCustom.MismatchCount;
        BlastNCBI.Parameters.OutputCustom.IdentityCount;BlastNCBI.Parameters.OutputCustom.PositiveScoringMatchCount;BlastNCBI.Parameters.OutputCustom.Evalue;
        BlastNCBI.Parameters.OutputCustom.Bitscore;
    ] |> List.toSeq

(**
Finally, we create a `BlastParam` of the type `OutputTypeCustom` from a touple of `outputType` and `outputFormat`.
_Note: No touple required if you want to use the default output format. If this is the case,
just create a `BlastParam` of type `OutputType`.  
*)

///The final output format
let customOutputFormat = OutputTypeCustom(outputType , outputFormat)

(**
We now have everything set up to perform the BLAST search. As we are talking about proteins, we will use blastP. The parameters needed for the Wrapper function are:
    - path of the ncbi/bin folder
    - path and name of the search database 
    - path and name of the query
    - path and name of our output file
    - a sequence of BLAST parameters, containing any parameters additional to the above (like our customOutputFormat)

_Note: in this case we can use the string `inputFile` that we used above for creating our database, as we did not specify another path or name for our database. Adjust accordingly if
done otherwise_
*)
///output file of the BLAST search
let outputPath = (__SOURCE_DIRECTORY__ + "/data/Output.txt") 

BlastWrapper(ncbiPath).blastP inputFile queryFastaPath outputPath ([customOutputFormat;] |> seq<BlastParams>)







