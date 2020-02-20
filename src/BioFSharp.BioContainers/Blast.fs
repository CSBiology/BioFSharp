namespace BioFSharp.BioContainers

module Blast =

    open FSharpAux
    open FSharpAux.IO
    open FSharpAux.IO.SchemaReader.Attribute
    open BioContainer
    open BioContainerIO

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


    type OutputType = 
        | Pairwise                        
        | Query_anchored                  
        | Query_anchored_NoIdentities     
        | Query_anchored_Flat             
        | Query_anchored_Flat_NoIdentities
        | XML                             
        | Tabular                         
        | TabularWithComments             
        | TextASN1                        
        | BinaryASN1                      
        | CSV                             
        | BLAST_ArchiveFormat             
        | JSON_Seqalign                   
        | JSON_Blast                      
        | XML2_Blast                      

            static member make = function 
                | Pairwise                          ->  0
                | Query_anchored                    ->  1
                | Query_anchored_NoIdentities       ->  2
                | Query_anchored_Flat               ->  3
                | Query_anchored_Flat_NoIdentities  ->  4
                | XML                               ->  5
                | Tabular                           ->  6
                | TabularWithComments               ->  7
                | TextASN1                          ->  8
                | BinaryASN1                        ->  9
                | CSV                               -> 10
                | BLAST_ArchiveFormat               -> 11
                | JSON_Seqalign                     -> 12
                | JSON_Blast                        -> 13
                | XML2_Blast                        -> 14

    //When not provided, the default value is:
    //'qseqid sseqid pident length mismatch gapopen qstart qend sstart send
    //evalue bitscore', which is equivalent to the keyword 'std'
    type OutputCustom = 
        | Query_SeqId               
        | Query_GI                  
        | Query_Accesion            
        | Query_Accesion_Version    
        | Query_Length              
        | Subject_SeqId             
        | Subject_All_SeqIds        
        | Subject_GI                
        | Subject_All_GIs           
        | Subject_Accession         
        | Subject_Accession_Version 
        | Subject_All_Accession     
        | Subject_Length            
        | Query_StartOfAlignment    
        | Query_EndOfAlignment      
        | Subject_StartOfAlignment  
        | Subject_EndOfAlignment    
        | Query_AlignedPartOf       
        | Subject_AlignedPartOf     
        | Evalue                    
        | Bitscore                  
        | RawScore                  
        | AlignmentLength           
        | Identity                  
        | IdentityCount             
        | MismatchCount             
        | PositiveScoringMatchCount 
        | GapOpeningCount           
        | GapCount                  
        | PositiveScoringMatch      
        //means Query and subject frames separated by a '/'
        | Frames                   
        | Query_Frames             
        | Subject_Frames           
        //means Blast traceback operations (BTOP)
        | BTOP                      
        | Subject_TaxonomyIDs       
        | Subject_Scientific_Names  
        | Subject_Common_Names      
        | Subject_Blast_Names       
        | Subject_Super_Kingdoms    
        | Subject_Title             
        | Subject_All_Titles        
        | Subject_Strand            
        | Query_CoveragePerSubject  
        | Query_CoveragePerHSP    

        static member make = function
            | Query_SeqId               -> "qseqid"
            | Query_GI                  -> "qgi"
            | Query_Accesion            -> "qacc"
            | Query_Accesion_Version    -> "qaccver"
            | Query_Length              -> "qlen"
            | Subject_SeqId             -> "sseqid"
            | Subject_All_SeqIds        -> "sallseqid"
            | Subject_GI                -> "sgi"
            | Subject_All_GIs           -> "sallgi"
            | Subject_Accession         -> "sacc"
            | Subject_Accession_Version -> "saccver"
            | Subject_All_Accession     -> "sallacc"
            | Subject_Length            -> "slen"
            | Query_StartOfAlignment    -> "qstart"
            | Query_EndOfAlignment      -> "qend"
            | Subject_StartOfAlignment  -> "sstart"
            | Subject_EndOfAlignment    -> "send"
            | Query_AlignedPartOf       -> "qseq"
            | Subject_AlignedPartOf     -> "sseq" 
            | Evalue                    -> "evalue"
            | Bitscore                  -> "bitscore"
            | RawScore                  -> "score"
            | AlignmentLength           -> "length"
            | Identity                  -> "pident" 
            | IdentityCount             -> "nident"
            | MismatchCount             -> "mismatch"
            | PositiveScoringMatchCount -> "positive"
            | GapOpeningCount           -> "gapopen"
            | GapCount                  -> "gaps"
            | PositiveScoringMatch      -> "ppos"
            //means Query and subject frames separated by a '/'
            | Frames                    -> "frames" 
            | Query_Frames              -> "qframe"
            | Subject_Frames            -> "sframe"
            //means Blast traceback operations (BTOP)
            | BTOP                      -> "btop" 
            | Subject_TaxonomyIDs       -> "staxids" 
            | Subject_Scientific_Names  -> "sscinames"
            | Subject_Common_Names      -> "scomnames"
            | Subject_Blast_Names       -> "sblastnames"
            | Subject_Super_Kingdoms    -> "sskingdoms"
            | Subject_Title             -> "stitle"
            | Subject_All_Titles        -> "salltitles"
            | Subject_Strand            -> "sstrand"
            | Query_CoveragePerSubject  -> "qcovs"
            | Query_CoveragePerHSP      -> "qcovhsp"


    type BlastParams =
        | SearchDB of string
        | Query    of string
        | Output   of string
        | OutputType of OutputType
        | OutputTypeCustom of OutputType * seq<OutputCustom>
        | Num_threads of int
        | Max_Hits of int

        static member makeCmd  = function
            | SearchDB  (path)      -> ["-db"    ; path]
            | Query     (path)      -> ["-query" ; path]
            | Output    (path)      -> ["-out"   ; path]
            | OutputType(format)    -> ["-outfmt"; string (format |> OutputType.make)]
            | OutputTypeCustom(t,p) ->  let tmp = 
                                            p 
                                            |> Seq.map OutputCustom.make 
                                            |> String.concat " "
                                        match t with
                                        | OutputType.Tabular             -> ["-outfmt"; sprintf "%s %s" (string (t |> OutputType.make)) tmp]
                                        | OutputType.TabularWithComments -> ["-outfmt"; sprintf "%s %s" (string (t |> OutputType.make)) tmp]
                                        | OutputType.CSV                 -> ["-outfmt"; sprintf "%s %s" (string (t |> OutputType.make)) tmp]
                                        | _ -> failwithf "Output format %A does not support custom columns." t                                
            | Num_threads(i)        -> ["-num_threads"; string i]
            | Max_Hits (i)          -> ["-max_target_seqs"; string i]

        static member makeCmdWith (m: MountInfo) = function
            | SearchDB  (path)      -> ["-db"    ; (MountInfo.containerPathOf m path)]
            | Query     (path)      -> ["-query" ; (MountInfo.containerPathOf m path)]
            | Output    (path)      -> ["-out"   ; (MountInfo.containerPathOf m path)]
            | OutputType(format)    -> ["-outfmt"; string (format |> OutputType.make)]
            | OutputTypeCustom(t,p) ->  let tmp = 
                                            p 
                                            |> Seq.map OutputCustom.make 
                                            |> String.concat " "
                                        match t with
                                        | OutputType.Tabular             -> ["-outfmt"; sprintf "%s %s" (string (t |> OutputType.make)) tmp]
                                        | OutputType.TabularWithComments -> ["-outfmt"; sprintf "%s %s" (string (t |> OutputType.make)) tmp]
                                        | OutputType.CSV                 -> ["-outfmt"; sprintf "%s %s" (string (t |> OutputType.make)) tmp]
                                        | _ -> failwithf "Output format %A does not support custom columns." t                                
            | Num_threads(i)        -> ["-num_threads"; string i]
            | Max_Hits (i)          -> ["-max_target_seqs"; string i]


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

    let runBlastPAsync (bcContext:BioContainer.BcContext) (opt:BlastParams list) = 
        let cmds = (opt |> List.map (BlastParams.makeCmdWith bcContext.Mount))
        let tp = "blastp"::(cmds |> List.concat)

        printfn "Starting process blastp\r\nparameters:"
        cmds |> List.iter (fun op -> printfn "\t%s" (String.concat " " op))

        async {
                let! res = BioContainer.execAsync bcContext tp           
                return res
        }

    let runBlastP (bcContext:BioContainer.BcContext) (opt:BlastParams list) = 
        runBlastPAsync bcContext opt
        |> Async.RunSynchronously

    let runBlastNAsync (bcContext:BioContainer.BcContext) (opt:BlastParams list) = 
        let cmds = (opt |> List.map (BlastParams.makeCmdWith bcContext.Mount))
        let tp = "blastn"::(cmds |> List.concat)

        printfn "Starting process blastn\r\nparameters:"
        cmds |> List.iter (fun op -> printfn "\t%s" (String.concat " " op))

        async {
                let! res = BioContainer.execAsync bcContext tp           
                return res
 
        }

    let runBlastN (bcContext:BioContainer.BcContext) (opt:BlastParams list) =
        runBlastNAsync bcContext opt
        |> Async.RunSynchronously