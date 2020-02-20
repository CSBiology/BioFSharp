(*** hide ***)
#I @"../../bin/BioFSharp/net47/"
#I @"../../bin/BioFSharp.BioDB/net45/"
#I @"../../bin/BioFSharp.ImgP/net47"
#I @"../../bin/BioFSharp.IO/net47/"
#I @"../../bin/BioFSharp.Parallel/net47/"
#I @"../../bin/BioFSharp.Stats/net47/"
#I @"../../bin/BioFSharp.Vis/net47/"
#r @"../../lib/Formatting/FSharp.Plotly.dll"
#I @"../../bin/BioFSharp.BioContainers/net47/"
#r @"C:\Users\kevin\source\repos\CSBiology\BioFSharp\packages\Docker.DotNet\lib\netstandard2.0\Docker.DotNet.dll"
#r "BioFSharp.dll"
#r "BioFSharp.BioContainers.dll"

open BioFSharp.BioContainers
open BioFSharp.BioContainers.BioContainer
(**

Designing F# APIs for biocontainers
===================================

This page is a design suggestion for common coding practice for all biocontainer APIs used in this library.
Do not take it as ultima ratio, but as a guideline that is up for community discussion.

Biocontainer targets
--------------------

If you want to create an F# API for a bioinformatic tool, make sure to check if the tool is already containerized
in the [BioConstainers registry](https://biocontainers.pro/#/registry) or [repository](https://github.com/BioContainers/containers). If not, make sure to create your own image according to the [BioConstainers standards](https://biocontainers-edu.biocontainers.pro/en/latest/what_is_biocontainers.html).

Suggested BioContainer API design workflow
------------------------------------------

- **Pull or build the target container.**
- **Run the help command in the container**
That way you can check that the container is working and you get the full list of commands with additional information.
You can alternatively get the description of commands from the documentation of the containerized tool.
- **Create a DSL for the command line arguments as follows:**
- **Create functions to run commands in the container**

Here is an example walkthrough for the `makeblastdb` tool. The documentation used to create the DSL can be found [here](https://www.ncbi.nlm.nih.gov/books/NBK279684/)

The relevant arguments are:

![MakeBlastDBParams](img/MakeBlastDBParams.png)

###Use Discriminated Union types for all types in the domain.
The top level type should be named "[toolname]Params"
For arguments with various input ranges, choose a fitting type at will, e.g. `String` for a title, `int` for number of cores, etc. or use the type annotations from the tool's docs when provided
*)

///DSL for command line arguments for the NCBI makeblastdb tool
type MakeBlastDbParams =
///Input file/database name
| Input of string

(**
###For flag type arguments, use typeless union labels**
*)

type MakeBlastDbParams =
    ///Input file/database name
    | Input         of string
    ///Parse bar delimited sequence identifiers (e.g., gi|129295) in FASTA input
    | ParseSeqIds    

(**
###For arguments than can only have a defined set of values, use a dedicated discriminated union type**
*)

///Input file type for makeblastdb
type MakeBlastDBInputType =
    ///fasta: for FASTA file(s)
    |Fasta
    ///blastdb: for BLAST database(s)
    |Blastdb
    ///asn1_txt: for Seq-entries in text ASN.1 format
    |Asn1Txt
    ///asn1_bin: for Seq-entries in binary ASN.1 format
    |ASN1Bin

type MakeBlastDbParams =
    ///Input file/database name
    | Input         of string
    ///Input file type for makeblastdb
    | InputType     of MakeBlastDBInputType
    ///Parse bar delimited sequence identifiers (e.g., gi|129295) in FASTA input
    | ParseSeqIds    


(**
###For each discriminated union type you have, create converter functions:

Sub union types:

- make: a function that returns the argument converted from DSL to string. This function can be used for creating commands usable in any local shell.
- make: a function that returns the argument converted from DSL to string, using the `MountInfo.containerPathOf` function. 
    This function creates commands that are usable in a container with mounted directories and ensures access for the tool to your mounted input/output files. You only need this function for types that contain paths.

Top level union type:

- makeCmd: a function that returns the argument converted from DSL to string, preceded with the option indicator (e.g. --, -). This function can be used for creating commands usable in any local shell.
- makeCmdWith: a function that returns the argument converted from DSL to string, preceded with the option indicator (e.g. --, -), using the `MountInfo.containerPathOf` function. 
    This function creates commands that are usable in a container with mounted directories and ensures access for the tool to your mounted input/output files. You only need this function for types that contain paths.

When containing subunions, the converter for the top level union should call the converter for the subunion for argument string creation.
All return values of the top level union should be wrapped in a list.
*)

///Input file type for makeblastdb
type MakeBlastDBInputType =
    ///fasta: for FASTA file(s)
    |Fasta
    ///blastdb: for BLAST database(s)
    |Blastdb
    ///asn1_txt: for Seq-entries in text ASN.1 format
    |Asn1Txt
    ///asn1_bin: for Seq-entries in binary ASN.1 format
    |ASN1Bin
    static member make = function
        |Fasta      -> "fasta"
        |Blastdb    -> "blastdb"
        |Asn1Txt    -> "asn1_txt"
        |ASN1Bin    -> "asn1_bin"

type MakeBlastDbParams =
    ///Input file/database name
    | Input         of string
    ///Input file type for makeblastdb
    | InputType     of MakeBlastDBInputType
    ///Parse bar delimited sequence identifiers (e.g., gi|129295) in FASTA input
    | ParseSeqIds    

    static member makeCmd = function
    | Input  (path)         -> ["-in"           ; path]
    | InputType it          -> ["-input_type"   ; MakeBlastDBInputType.make it]
    | ParseSeqIds           -> ["-parse_seqids"] 

    ///returns the string form of command line argument DSL for makeblastdb with paths adjusted for container localization
    static member makeCmdWith (m: MountInfo) = function
    | Input  (path)         -> ["-in"           ; MountInfo.containerPathOf m path]
    | InputType it          -> ["-input_type"   ; MakeBlastDBInputType.make it]
    | ParseSeqIds           -> ["-parse_seqids"] 
    

(**
###Create functions for in-container command execution
Always create a synchronous and asynchronous version:
*)

open BioFSharp.BioContainers
open BioFSharp.BioContainers.BioContainer

let runMakeBlastDBAsync (bcContext:BioContainer.BcContext) (opt:MakeBlastDbParams list) = 

    //create correct command line strings from the input parameters
    let cmds = (opt |> List.map (MakeBlastDbParams.makeCmdWith bcContext.Mount))

    //append the commands after the running command, most time this is the tool name.
    let tp = "makeblastdb"::(cmds |> List.concat)

    printfn "Starting process makeblastdb\r\nparameters:"
    cmds |> List.iter (fun op -> printfn "\t%s" (String.concat " " op))

    //await execution of the commands in the container. execAsync does not return the result, use execReturn for that.
    async {
            let! res = BioContainer.execAsync bcContext tp           
            return res
    }

//synchronous version
let runMakeBlastDB (bcContext:BioContainer.BcContext) (opt:MakeBlastDbParams list) =

    runMakeBlastDBAsync bcContext opt
    |> Async.RunSynchronously


(**
Full example
------------
Here is the full DSL for makeblastdb:
*)


open BioFSharp.BioContainers
open BioFSharp.BioContainers.BioContainer

///Input file type for makeblastdb
type MakeBlastDBInputType =
    ///fasta: for FASTA file(s)
    |Fasta
    ///blastdb: for BLAST database(s)
    |Blastdb
    ///asn1_txt: for Seq-entries in text ASN.1 format
    |Asn1Txt
    ///asn1_bin: for Seq-entries in binary ASN.1 format
    |ASN1Bin

    static member make = function
        |Fasta      -> "fasta"
        |Blastdb    -> "blastdb"
        |Asn1Txt    -> "asn1_txt"
        |ASN1Bin    -> "asn1_bin"

///Molecule type of input, values can be nucl or prot
type DbType =
    | Protein 
    | Nucleotide

    static member make = function
        | Protein       -> "prot"
        | Nucleotide    -> "nucl"

///DSL for command line arguments for the NCBI makeblastdb tool
type MakeBlastDbParams =
    ///Input file/database name
    | Input         of string
    ///Input file type for makeblastdb
    | InputType     of MakeBlastDBInputType
    ///Molecule type of input, values can be nucl or prot
    | DbType        of DbType
    ///Title for BLAST database. If not set, the input file name will be used
    | Title         of string
    ///Parse bar delimited sequence identifiers (e.g., gi|129295) in FASTA input
    | ParseSeqIds    
    ///Create index of sequence hash values
    | HashIndex
    ///Comma-separated list of input files containing masking data as produced by NCBI masking applications (e.g. dustmasker, segmasker, windowmasker
    | MaskData      of string list
    ///Name of BLAST database to be created. Input file name is used if none provided. This field is required if input consists of multiple files
    | Output        of string
    ///Maximum file size to use for BLAST database. 4GB is the maximum supported by the database structure
    | MaxFileSize   of string
    ///Taxonomy ID to assign to all sequences.
    | TaxId         of int
    ///File with two columns mapping sequence ID to the taxonomy ID. The first column is the sequence ID represented as one of:
    ///
    ///1
    ///fasta with accessions (e.g., emb|X17276.1|)
    ///
    ///2
    ///fasta with GI (e.g., gi|4)
    ///
    ///3
    ///GI as a bare number (e.g., 4)
    ///
    ///4
    ///A local ID. The local ID must be prefixed with "lcl" (e.g., lcl|4).
    ///The second column should be the NCBI taxonomy ID (e.g., 9606 for human).
    | TaxIdMapFile  of string
    ///Program log file (default is stderr).
    | Logfile       of string

    ///returns the string form of command line argument DSL for makeblastdb
    static member makeCmd = function
        | Input  (path)         -> ["-in"           ; path]
        | InputType it          -> ["-input_type"   ; MakeBlastDBInputType.make it]
        | DbType (dbt)          -> ["-dbtype"       ; DbType.make dbt]
        | Title t               -> ["-title"        ; t]
        | ParseSeqIds           -> ["-parse_seqids"] 
        | HashIndex             -> ["-hash_index"]
        | MaskData (paths)      -> ["-mask_data"    ; paths |> String.concat ","]
        | Output (path)         -> ["-out"          ; path]
        | MaxFileSize fs        -> ["-max_file_size"; fs]
        | TaxId tid             -> ["-taxid"        ; sprintf "%i" tid]
        | TaxIdMapFile (path)   -> ["-taxid_map"    ; path]
        | Logfile(path)         -> ["-logfile"      ; path]

    ///returns the string form of command line argument DSL for makeblastdb with paths adjusted for container localization
    static member makeCmdWith (m: MountInfo) = function
        | Input  (path)         -> ["-in"           ; MountInfo.containerPathOf m path]
        | InputType it          -> ["-input_type"   ; MakeBlastDBInputType.make it]
        | DbType (dbt)          -> ["-dbtype"       ; DbType.make dbt]
        | Title t               -> ["-title"        ; t]
        | ParseSeqIds           -> ["-parse_seqids"] 
        | HashIndex             -> ["-hash_index"]
        | MaskData (paths)      -> ["-mask_data"    ; paths |> List.map (MountInfo.containerPathOf m) |> String.concat ","]
        | Output (path)         -> ["-out"          ; MountInfo.containerPathOf m path]
        | MaxFileSize fs        -> ["-max_file_size"; fs]
        | TaxId tid             -> ["-taxid"        ; sprintf "%i" tid]
        | TaxIdMapFile (path)   -> ["-taxid_map"    ; MountInfo.containerPathOf m path]
        | Logfile(path)         -> ["-logfile"      ; MountInfo.containerPathOf m path]

let runMakeBlastDBAsync (bcContext:BioContainer.BcContext) (opt:MakeBlastDbParams list) = 

    let cmds = (opt |> List.map (MakeBlastDbParams.makeCmdWith bcContext.Mount))
    let tp = "makeblastdb"::(cmds |> List.concat)

    printfn "Starting process makeblastdb\r\nparameters:"
    cmds |> List.iter (fun op -> printfn "\t%s" (String.concat " " op))

    async {
            let! res = BioContainer.execAsync bcContext tp           
            return res
    }

let runMakeBlastDB (bcContext:BioContainer.BcContext) (opt:MakeBlastDbParams list) =

    runMakeBlastDBAsync bcContext opt
    |> Async.RunSynchronously