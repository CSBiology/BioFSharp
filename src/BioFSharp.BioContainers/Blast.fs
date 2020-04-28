namespace BioFSharp.BioContainers

//BioContainer tag  : blast:2.2.31--pl526h3066fca_3
//BioContainer pull : docker pull quay.io/biocontainers/blast:2.2.31--pl526h3066fca_3


//TODO implement all commands from the manual
//https://www.ncbi.nlm.nih.gov/books/NBK279684/

///DSLs for executing commands in the BLAST biocontainer 
///
///Generated and tested for the tag blast:2.2.31--pl526h3066fca_3
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
    
    ///returns an asynchronous computation that will run makeblastdb in the specified container context with the given commands
    let runMakeBlastDBAsync (bcContext:BioContainer.BcContext) (opt:MakeBlastDbParams list) = 
    
        let cmds = (opt |> List.map (MakeBlastDbParams.makeCmdWith bcContext.Mount))
        let tp = "makeblastdb"::(cmds |> List.concat)
    
        printfn "Starting process makeblastdb\r\nparameters:"
        cmds |> List.iter (fun op -> printfn "\t%s" (String.concat " " op))
    
        async {
                let! res = BioContainer.execAsync bcContext tp           
                return res
        }
    
    ///run makeblastdb in the specified container context with the given commands
    let runMakeBlastDB (bcContext:BioContainer.BcContext) (opt:MakeBlastDbParams list) =
    
        runMakeBlastDBAsync bcContext opt
        |> Async.RunSynchronously

    ///alignment view options
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

    ///OutputType options Tabular, TabularWithComments, and CSV can be additionally configured to produce a custom format specified by space delimited format specifiers.
    ///When not provided, the default value is:
    ///'qseqid sseqid pident length mismatch gapopen qstart qend sstart send
    ///evalue bitscore', which is equivalent to the keyword 'std'
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


    ///DSL for command line parameters that are common to all blast applications
    type BlastParams =
        ///BLAST database name.
        | SearchDB              of string
        //Query file name.
        | Query                 of string
        //Output file name
        | Output                of string
        | OutputType            of OutputType
        | OutputTypeCustom      of OutputType * seq<OutputCustom>
        // Number of threads (CPUs) to use in blast search.
        | NumThreads            of int
        //Number of aligned sequences to keep. Use with report formats that do not have separate definition line and alignment sections such as tabular (all outfmt > 4). Not compatible with num_descriptions or num_alignments. Ties are broken by order of sequences in the database.
        | MaxHits               of int
        ///Location on the query sequence (Format: start-stop)
        |QueryLocation          of (int*int)
        ///Expect value (E) for saving hits
        |EValue                 of float
        ///File with subject sequence(s) to search.
        |Subject                of string
        ///Location on the subject sequence (Format: start-stop).
        |SubjectLocation        of (int*int)
        ///Show NCBI GIs in report.
        |ShowGIs
        ///Show one-line descriptions for this number of database sequences.
        |Num_Descriptions       of int
        ///Show alignments for this number of database sequences.
        |NumAlignments          of int
        ///Maximum number of HSPs (alignments) to keep for any single query-subject pair. The HSPs shown will be the best as judged by expect value. This number should be an int that is one or greater. If this option is not set, BLAST shows all HSPs meeting the expect value criteria. Setting it to one will show only the best HSP for every query-subject pair
        |MaxHSPs                of int
        ///Produce HTML output
        |HTML 
        ///Restrict search of database to GI’s listed in this file. Local searches only.
        |GIList of string
        ///Restrict search of database to everything except the GI’s listed in this file. Local searches only.
        |NegativeGIList of string
        ///Restrict search with the given Entrez query. Remote searches only.
        |EntrezQuery of string
        ///Delete a hit that is enveloped by at least this many higher-scoring hits.
        |CullingLimit of int
        ///Best Hit algorithm overhang value (recommended value: 0.1)
        |BestHitOverhang of float
        ///Best Hit algorithm score edge value (recommended value: 0.1)
        |BestHitScoreEdge of float
        ///Effective size of the database
        |DBSize  of int
        ///Effective length of the search space
        |SearchSpaceLength of int
        ///Search strategy file to read.
        |ImportSearchStrategy    of string
        ///Record search strategy to this file.
        |ExportSearchStrategy of string
        ///Parse query and subject bar delimited sequence identifiers (e.g., gi|129295).
        |ParseDeflines            
        ///Execute search on NCBI servers?
        |Remote                    

        static member makeCmd  = function
            | SearchDB          (path)      -> ["-db"    ; path]
            | Query             (path)      -> ["-query" ; path]
            | Output            (path)      -> ["-out"   ; path]
            | OutputType        (format)    -> ["-outfmt"; string (format |> OutputType.make)]
            | OutputTypeCustom  (t,p)       -> 
                let tmp = 
                    p 
                    |> Seq.map OutputCustom.make 
                    |> String.concat " "
                match t with
                | OutputType.Tabular             -> ["-outfmt"; sprintf "%s %s" (string (t |> OutputType.make)) tmp]
                | OutputType.TabularWithComments -> ["-outfmt"; sprintf "%s %s" (string (t |> OutputType.make)) tmp]
                | OutputType.CSV                 -> ["-outfmt"; sprintf "%s %s" (string (t |> OutputType.make)) tmp]
                | _ -> failwithf "Output format %A does not support custom columns." t                                

            | NumThreads            i       -> ["-num_threads";             string i]
            | MaxHits               i       -> ["-max_target_seqs";         string i]
            | QueryLocation         (s,e)   -> ["-query_loc";               sprintf "%i-%i" s e]
            | EValue                e       -> ["-evalue";                  string e]
            | Subject               path    -> ["-subject";                 path]
            | SubjectLocation       (s,e)   -> ["-subject_loc";             sprintf "%i-%i" s e]
            | ShowGIs                       -> ["-show_gis"]
            | Num_Descriptions      p       -> ["-num_descriptions";        string p]
            | NumAlignments         p       -> ["-num_alignments";          string p]
            | MaxHSPs               p       -> ["-max_hsps";                string p]
            | HTML                          -> ["-html"]
            | GIList                path    -> ["-gilist";                  path]
            | NegativeGIList        path    -> ["-negative_gilist";         path]
            | EntrezQuery           p       -> ["-entrez_query";            p]
            | CullingLimit          p       -> ["-culling_limit";           string p]
            | BestHitOverhang       p       -> ["-best_hit_overhang";       string p]
            | BestHitScoreEdge      p       -> ["-best_hit_score_edge";     string p]
            | DBSize                p       -> ["-dbsize";                  string p]
            | SearchSpaceLength     p       -> ["-searchsp";                string p]
            | ImportSearchStrategy  path    -> ["-import_search_strategy";  path]
            | ExportSearchStrategy  path    -> ["-export_search_strategy";  path]
            | ParseDeflines                 -> ["-parse_deflines"]
            | Remote                        -> ["-remote"]


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
            | NumThreads            i       -> ["-num_threads";             string i]
            | MaxHits               i       -> ["-max_target_seqs";         string i]
            | QueryLocation         (s,e)   -> ["-query_loc";               sprintf "%i-%i" s e]
            | EValue                e       -> ["-evalue";                  string e]
            | Subject               path    -> ["-subject";                 (MountInfo.containerPathOf m path)]
            | SubjectLocation       (s,e)   -> ["-subject_loc";             sprintf "%i-%i" s e]
            | ShowGIs                       -> ["-show_gis"]
            | Num_Descriptions      p       -> ["-num_descriptions";        string p]
            | NumAlignments         p       -> ["-num_alignments";          string p]
            | MaxHSPs               p       -> ["-max_hsps";                string p]
            | HTML                          -> ["-html"]
            | GIList                path    -> ["-gilist";                  (MountInfo.containerPathOf m path)]
            | NegativeGIList        path    -> ["-negative_gilist";         (MountInfo.containerPathOf m path)]
            | EntrezQuery           p       -> ["-entrez_query";            p]
            | CullingLimit          p       -> ["-culling_limit";           string p]
            | BestHitOverhang       p       -> ["-best_hit_overhang";       string p]
            | BestHitScoreEdge      p       -> ["-best_hit_score_edge";     string p]
            | DBSize                p       -> ["-dbsize";                  string p]
            | SearchSpaceLength     p       -> ["-searchsp";                string p]
            | ImportSearchStrategy  path    -> ["-import_search_strategy";  (MountInfo.containerPathOf m path)]
            | ExportSearchStrategy  path    -> ["-export_search_strategy";  (MountInfo.containerPathOf m path)]
            | ParseDeflines                 -> ["-parse_deflines"]
            | Remote                        -> ["-remote"]
    
    ///DSL for blastn programs
    ///The blastn application searches a nucleotide query against nucleotide subject sequences or a nucleotide database. 
    ///Four different tasks are supported: 
    ///
    ///1.) “megablast”, for very similar sequences (e.g, sequencing errors)
    ///
    ///2.) “dc-megablast”, typically used for inter-species comparisons
    ///
    ///3.) “blastn”, the traditional program used for inter-species comparisons
    ///
    ///4.) “blastn-short”, optimized for sequences less than 30 nucleotides.
    [<RequireQualifiedAccess>]
    module BlastN =
        
        ///DSL fo the blastn 'megablast' task command line options
        ///megablast is usually used for very similar sequences (e.g, sequencing errors)
        type MegablastParams = 
            ///Length of initial exact match.
            | WordSize                  of int
            ///Cost to open a gap. See appendix “BLASTN reward/penalty values”.
            | GapOpen                   of int
            ///gapextend        
            | GapExtend                 of int
            ///Reward for a nucleotide match.
            | Reward                    of int
            ///Penalty for a nucleotide mismatch.
            | Penalty                   of int
            ///Use MegaBLAST database index. Indices may be created with the makembindex application.
            | UseIndex                  of bool
            ///MegaBLAST database index name.
            | IndexName                 of string
            ///Use non-greedy dynamic programming extension.
            | NoGreedy                         
            //TODO refactor into proper type
            ///Query strand(s) to search against database/subject. Choice of both, minus, or plus.
            | Strand                    of string 
            ///Filter query sequence with dust.
            | Dust                      of string 
            ///Mask query using the sequences in this database.
            | FilteringDB               of string 
            ///Enable WindowMasker filtering using a Taxonomic ID.
            | WindowMaskerTaxId         of int    
            ///Enable WindowMasker filtering using this file.
            | WindowMaskerDB            of string 
            ///Apply filtering locations as soft masks (i.e., only for finding initial matches).
            | SoftMasking               of bool   
            ///Use lower case filtering in query and subject sequence(s).
            | LowerCaseMasking                      
            ///Filtering algorithm ID to apply to the BLAST database as soft mask (i.e., only for finding initial matches).
            | DBSoftMask                of int    
            ///Filtering algorithm ID to apply to the BLAST database as hard mask (i.e., sequence is masked for all phases of search).
            | DBHardMask                of int    
            ///Percent identity cutoff.
            | PercIdentity              of int    
            ///Heuristic value (in bits) for ungapped extensions.
            | XDropUngap                of float  
            ///Heuristic value (in bits) for preliminary gapped extensions.
            | XDropGap                  of float  
            /// Heuristic value (in bits) for final gapped alignment.
            | XDropGapFinal             of float  
            ///Minimum raw gapped score to keep an alignment in the preliminary gapped and trace-back stages. Normally set based upon expect value.
            | MinRawGappedScore         of int    
            ///Perform ungapped alignment.
            | Ungapped                           

            static member makeCmd = function
                | WordSize               p   -> ["-word_size"            ; string p]
                | GapOpen                p   -> ["-gapopen"              ; string p]
                | GapExtend              p   -> ["-gapextend"            ; string p]
                | Reward                 p   -> ["-reward"               ; string p]
                | Penalty                p   -> ["-penalty"              ; string p]
                | UseIndex               p   -> ["-use_index"            ; string p]
                | IndexName              path-> ["-index_name"           ; path]
                | NoGreedy                   -> ["-no_greedy"] 
                | Strand                 p   -> ["-strand"               ; p]
                | Dust                   p   -> ["-dust"                 ; p]
                | FilteringDB            path-> ["-filtering_db"         ; path]
                | WindowMaskerTaxId      p   -> ["-window_masker_taxid"  ; string p]
                | WindowMaskerDB         path-> ["-window_masker_db"     ; path]
                | SoftMasking            p   -> ["-soft_masking"         ; string p]
                | LowerCaseMasking           -> ["-lcase_masking"]
                | DBSoftMask             p   -> ["-db_soft_mask"         ; string p]
                | DBHardMask             p   -> ["-db_hard_mask"         ; string p]
                | PercIdentity           p   -> ["-perc_identity"        ; string p]
                | XDropUngap             p   -> ["-xdrop_ungap"          ; string p]
                | XDropGap               p   -> ["-xdrop_gap"            ; string p]
                | XDropGapFinal          p   -> ["-xdrop_gap_final"      ; string p]
                | MinRawGappedScore      p   -> ["-min_raw_gapped_score" ; string p]
                | Ungapped                   -> ["-ungapped"]
        
            static member makeCmdWith (m:MountInfo) = function
                | WordSize               p   -> ["-word_size"            ; string p]
                | GapOpen                p   -> ["-gapopen"              ; string p]
                | GapExtend              p   -> ["-gapextend"            ; string p]
                | Reward                 p   -> ["-reward"               ; string p]
                | Penalty                p   -> ["-penalty"              ; string p]
                | UseIndex               p   -> ["-use_index"            ; string p]
                | IndexName              path-> ["-index_name"           ; (MountInfo.containerPathOf m path)]
                | NoGreedy                   -> ["-no_greedy"] 
                | Strand                 p   -> ["-strand"               ; p]
                | Dust                   p   -> ["-dust"                 ; p]
                | FilteringDB            path-> ["-filtering_db"         ; (MountInfo.containerPathOf m path)]
                | WindowMaskerTaxId      p   -> ["-window_masker_taxid"  ; string p]
                | WindowMaskerDB         path-> ["-window_masker_db"     ; (MountInfo.containerPathOf m path)]
                | SoftMasking            p   -> ["-soft_masking"         ; string p]
                | LowerCaseMasking           -> ["-lcase_masking"]
                | DBSoftMask             p   -> ["-db_soft_mask"         ; string p]
                | DBHardMask             p   -> ["-db_hard_mask"         ; string p]
                | PercIdentity           p   -> ["-perc_identity"        ; string p]
                | XDropUngap             p   -> ["-xdrop_ungap"          ; string p]
                | XDropGap               p   -> ["-xdrop_gap"            ; string p]
                | XDropGapFinal          p   -> ["-xdrop_gap_final"      ; string p]
                | MinRawGappedScore      p   -> ["-min_raw_gapped_score" ; string p]
                | Ungapped                   -> ["-ungapped"]

        ///DSL fo the blastn 'dc-megablast' task command line options
        ///dc-megablast is typically used for inter-species comparison
        type DCMegablastParams = 
            ///Number of matching nucleotides in initial match. dc-megablast allows non-consecutive letters to match.
            | WordSize                  of int    
            ///Discontiguous MegaBLAST template type. Allowed values are coding, optimal and coding_and_optimal.
            //TODO implement proper type
            | TemplateType              of string
            ///Discontiguous MegaBLAST template length.
            | TemplateLength            of int    
            ///Multiple hits window size, use 0 to specify 1-hit algorithm
            | WindowSize                of int
            ///Cost to open a gap. See appendix “BLASTN reward/penalty values”.
            | GapOpen                   of int    
            ///Cost to extend a gap. See appendix “BLASTN reward/penalty values”.
            | GapExtend                 of int   
            ///Reward for a nucleotide match.
            | Reward                    of int 
            ///Penalty for a nucleotide mismatch.
            | Penalty                   of int 
            //TODO refactor into proper type
            ///Query strand(s) to search against database/subject. Choice of both, minus, or plus.
            | Strand                    of string 
            ///Filter query sequence with dust.
            | Dust                      of string 
            ///Mask query using the sequences in this database.
            | FilteringDB               of string 
            ///Enable WindowMasker filtering using a Taxonomic ID.
            | WindowMaskerTaxId         of int    
            ///Enable WindowMasker filtering using this file.
            | WindowMaskerDB            of string 
            ///Apply filtering locations as soft masks (i.e., only for finding initial matches).
            | SoftMasking               of bool   
            ///Use lower case filtering in query and subject sequence(s).
            | LowerCaseMasking                      
            ///Filtering algorithm ID to apply to the BLAST database as soft mask (i.e., only for finding initial matches).
            | DBSoftMask                of int    
            ///Filtering algorithm ID to apply to the BLAST database as hard mask (i.e., sequence is masked for all phases of search).
            | DBHardMask                of int    
            ///Percent identity cutoff.
            | PercIdentity              of int    
            ///Heuristic value (in bits) for ungapped extensions.
            | XDropUngap                of float  
            ///Heuristic value (in bits) for preliminary gapped extensions.
            | XDropGap                  of float  
            /// Heuristic value (in bits) for final gapped alignment.
            | XDropGapFinal             of float  
            ///Minimum raw gapped score to keep an alignment in the preliminary gapped and trace-back stages. Normally set based upon expect value.
            | MinRawGappedScore         of int    
            ///Perform ungapped alignment.
            | Ungapped                           

            static member makeCmd = function
                | WordSize               p   -> ["-window_size"          ; string p]
                | TemplateType           p   -> ["-template_type"        ; p]
                | TemplateLength         p   -> ["-template_length"      ; string p]
                | WindowSize             p   -> ["-window_size"          ; string p]
                | GapOpen                p   -> ["-gapopen"              ; string p]
                | GapExtend              p   -> ["-gapextend"            ; string p]  
                | Reward                 p   -> ["-reward"               ; string p]
                | Penalty                p   -> ["-penalty"              ; string p]
                | Strand                 p   -> ["-strand"               ; p]
                | Dust                   p   -> ["-dust"                 ; p]
                | FilteringDB            path-> ["-filtering_db"         ; path]
                | WindowMaskerTaxId      p   -> ["-window_masker_taxid"  ; string p]
                | WindowMaskerDB         path-> ["-window_masker_db"     ; path]
                | SoftMasking            p   -> ["-soft_masking"         ; string p]
                | LowerCaseMasking           -> ["-lcase_masking"]
                | DBSoftMask             p   -> ["-db_soft_mask"         ; string p]
                | DBHardMask             p   -> ["-db_hard_mask"         ; string p]
                | PercIdentity           p   -> ["-perc_identity"        ; string p]
                | XDropUngap             p   -> ["-xdrop_ungap"          ; string p]
                | XDropGap               p   -> ["-xdrop_gap"            ; string p]
                | XDropGapFinal          p   -> ["-xdrop_gap_final"      ; string p]
                | MinRawGappedScore      p   -> ["-min_raw_gapped_score" ; string p]
                | Ungapped                   -> ["-ungapped"]
        
            static member makeCmdWith (m:MountInfo) = function
                | WordSize               p   -> ["-window_size"          ; string p]
                | TemplateType           p   -> ["-template_type"        ; p]
                | TemplateLength         p   -> ["-template_length"      ; string p]
                | WindowSize             p   -> ["-window_size"          ; string p]
                | GapOpen                p   -> ["-gapopen"              ; string p]
                | GapExtend              p   -> ["-gapextend"            ; string p]  
                | Reward                 p   -> ["-reward"               ; string p]
                | Penalty                p   -> ["-penalty"              ; string p]
                | Strand                 p   -> ["-strand"               ; p]
                | Dust                   p   -> ["-dust"                 ; p]
                | FilteringDB            path-> ["-filtering_db"         ; (MountInfo.containerPathOf m path)]
                | WindowMaskerTaxId      p   -> ["-window_masker_taxid"  ; string p]
                | WindowMaskerDB         path-> ["-window_masker_db"     ; (MountInfo.containerPathOf m path)]
                | SoftMasking            p   -> ["-soft_masking"         ; string p]
                | LowerCaseMasking           -> ["-lcase_masking"]
                | DBSoftMask             p   -> ["-db_soft_mask"         ; string p]
                | DBHardMask             p   -> ["-db_hard_mask"         ; string p]
                | PercIdentity           p   -> ["-perc_identity"        ; string p]
                | XDropUngap             p   -> ["-xdrop_ungap"          ; string p]
                | XDropGap               p   -> ["-xdrop_gap"            ; string p]
                | XDropGapFinal          p   -> ["-xdrop_gap_final"      ; string p]
                | MinRawGappedScore      p   -> ["-min_raw_gapped_score" ; string p]
                | Ungapped                   -> ["-ungapped"]

        ///DSL fo blastn command line options
        ///blastn is the traditional program used for inter-species comparisons
        type BlastNParams = 
            ///Length of initial exact match.
            | WordSize                  of int
            ///Cost to open a gap. See appendix “BLASTN reward/penalty values”.
            | GapOpen                   of int
            ///Cost to extend a gap. See appendix “BLASTN reward/penalty values”.
            | GapExtend                 of int   
            ///Reward for a nucleotide match.
            | Reward                    of int    
            ///Penalty for a nucleotide mismatch.
            | Penalty                   of int   
            ///Query strand(s) to search against database/subject. Choice of both, minus, or plus.
            | Strand                    of string 
            ///Filter query sequence with dust.
            | Dust                      of string 
            ///Mask query using the sequences in this database.
            | FilteringDB               of string 
            ///Enable WindowMasker filtering using a Taxonomic ID.
            | WindowMaskerTaxId         of int    
            ///Enable WindowMasker filtering using this file.
            | WindowMaskerDB            of string 
            ///Apply filtering locations as soft masks (i.e., only for finding initial matches).
            | SoftMasking               of bool   
            ///Use lower case filtering in query and subject sequence(s).
            | LowerCaseMasking                      
            ///Filtering algorithm ID to apply to the BLAST database as soft mask (i.e., only for finding initial matches).
            | DBSoftMask                of int    
            ///Filtering algorithm ID to apply to the BLAST database as hard mask (i.e., sequence is masked for all phases of search).
            | DBHardMask                of int    
            ///Percent identity cutoff.
            | PercIdentity              of int    
            ///Heuristic value (in bits) for ungapped extensions.
            | XDropUngap                of float  
            ///Heuristic value (in bits) for preliminary gapped extensions.
            | XDropGap                  of float  
            /// Heuristic value (in bits) for final gapped alignment.
            | XDropGapFinal             of float  
            ///Minimum raw gapped score to keep an alignment in the preliminary gapped and trace-back stages. Normally set based upon expect value.
            | MinRawGappedScore         of int    
            ///Perform ungapped alignment.
            | Ungapped                           

            static member makeCmd = function
                | WordSize               p   -> ["-word_size"            ; string p]
                | GapOpen                p   -> ["-gapopen"              ; string p]
                | GapExtend              p   -> ["-gapextend"            ; string p]
                | Reward                 p   -> ["-reward"               ; string p]
                | Penalty                p   -> ["-penalty"              ; string p]
                | Strand                 p   -> ["-strand"               ; p]
                | Dust                   p   -> ["-dust"                 ; p]
                | FilteringDB            path-> ["-filtering_db"         ; path]
                | WindowMaskerTaxId      p   -> ["-window_masker_taxid"  ; string p]
                | WindowMaskerDB         path-> ["-window_masker_db"     ; path]
                | SoftMasking            p   -> ["-soft_masking"         ; string p]
                | LowerCaseMasking           -> ["-lcase_masking"]
                | DBSoftMask             p   -> ["-db_soft_mask"         ; string p]
                | DBHardMask             p   -> ["-db_hard_mask"         ; string p]
                | PercIdentity           p   -> ["-perc_identity"        ; string p]
                | XDropUngap             p   -> ["-xdrop_ungap"          ; string p]
                | XDropGap               p   -> ["-xdrop_gap"            ; string p]
                | XDropGapFinal          p   -> ["-xdrop_gap_final"      ; string p]
                | MinRawGappedScore      p   -> ["-min_raw_gapped_score" ; string p]
                | Ungapped                   -> ["-ungapped"]
        
            static member makeCmdWith (m:MountInfo) = function
                | WordSize               p   -> ["-word_size"            ; string p]
                | GapOpen                p   -> ["-gapopen"              ; string p]
                | GapExtend              p   -> ["-gapextend"            ; string p]
                | Reward                 p   -> ["-reward"               ; string p]
                | Penalty                p   -> ["-penalty"              ; string p]
                | Strand                 p   -> ["-strand"               ; p]
                | Dust                   p   -> ["-dust"                 ; p]
                | FilteringDB            path-> ["-filtering_db"         ; (MountInfo.containerPathOf m path)]
                | WindowMaskerTaxId      p   -> ["-window_masker_taxid"  ; string p]
                | WindowMaskerDB         path-> ["-window_masker_db"     ; (MountInfo.containerPathOf m path)]
                | SoftMasking            p   -> ["-soft_masking"         ; string p]
                | LowerCaseMasking           -> ["-lcase_masking"]
                | DBSoftMask             p   -> ["-db_soft_mask"         ; string p]
                | DBHardMask             p   -> ["-db_hard_mask"         ; string p]
                | PercIdentity           p   -> ["-perc_identity"        ; string p]
                | XDropUngap             p   -> ["-xdrop_ungap"          ; string p]
                | XDropGap               p   -> ["-xdrop_gap"            ; string p]
                | XDropGapFinal          p   -> ["-xdrop_gap_final"      ; string p]
                | MinRawGappedScore      p   -> ["-min_raw_gapped_score" ; string p]
                | Ungapped                   -> ["-ungapped"]

        ///DSL fo the blastn 'blastn-short' task command line options
        ///blastn-short is optimized for sequences less than 30 nucleotides.
        type BlastNShortParams = 
            ///Length of initial exact match.
            | WordSize                  of int
            ///Cost to open a gap. See appendix “BLASTN reward/penalty values”.
            | GapOpen                   of int
            ///Cost to extend a gap. See appendix “BLASTN reward/penalty values”.
            | GapExtend                 of int   
            ///Reward for a nucleotide match.
            | Reward                    of int    
            ///Penalty for a nucleotide mismatch.
            | Penalty                   of int   
            ///Query strand(s) to search against database/subject. Choice of both, minus, or plus.
            | Strand                    of string 
            ///Filter query sequence with dust.
            | Dust                      of string 
            ///Mask query using the sequences in this database.
            | FilteringDB               of string 
            ///Enable WindowMasker filtering using a Taxonomic ID.
            | WindowMaskerTaxId         of int    
            ///Enable WindowMasker filtering using this file.
            | WindowMaskerDB            of string 
            ///Apply filtering locations as soft masks (i.e., only for finding initial matches).
            | SoftMasking               of bool   
            ///Use lower case filtering in query and subject sequence(s).
            | LowerCaseMasking                      
            ///Filtering algorithm ID to apply to the BLAST database as soft mask (i.e., only for finding initial matches).
            | DBSoftMask                of int    
            ///Filtering algorithm ID to apply to the BLAST database as hard mask (i.e., sequence is masked for all phases of search).
            | DBHardMask                of int    
            ///Percent identity cutoff.
            | PercIdentity              of int    
            ///Heuristic value (in bits) for ungapped extensions.
            | XDropUngap                of float  
            ///Heuristic value (in bits) for preliminary gapped extensions.
            | XDropGap                  of float  
            /// Heuristic value (in bits) for final gapped alignment.
            | XDropGapFinal             of float  
            ///Minimum raw gapped score to keep an alignment in the preliminary gapped and trace-back stages. Normally set based upon expect value.
            | MinRawGappedScore         of int    
            ///Perform ungapped alignment.
            | Ungapped                           

            static member makeCmd = function
                | WordSize               p   -> ["-word_size"            ; string p]
                | GapOpen                p   -> ["-gapopen"              ; string p]
                | GapExtend              p   -> ["-gapextend"            ; string p]
                | Reward                 p   -> ["-reward"               ; string p]
                | Penalty                p   -> ["-penalty"              ; string p]
                | Strand                 p   -> ["-strand"               ; p]
                | Dust                   p   -> ["-dust"                 ; p]
                | FilteringDB            path-> ["-filtering_db"         ; path]
                | WindowMaskerTaxId      p   -> ["-window_masker_taxid"  ; string p]
                | WindowMaskerDB         path-> ["-window_masker_db"     ; path]
                | SoftMasking            p   -> ["-soft_masking"         ; string p]
                | LowerCaseMasking           -> ["-lcase_masking"]
                | DBSoftMask             p   -> ["-db_soft_mask"         ; string p]
                | DBHardMask             p   -> ["-db_hard_mask"         ; string p]
                | PercIdentity           p   -> ["-perc_identity"        ; string p]
                | XDropUngap             p   -> ["-xdrop_ungap"          ; string p]
                | XDropGap               p   -> ["-xdrop_gap"            ; string p]
                | XDropGapFinal          p   -> ["-xdrop_gap_final"      ; string p]
                | MinRawGappedScore      p   -> ["-min_raw_gapped_score" ; string p]
                | Ungapped                   -> ["-ungapped"]
        
            static member makeCmdWith (m:MountInfo) = function
                | WordSize               p   -> ["-word_size"            ; string p]
                | GapOpen                p   -> ["-gapopen"              ; string p]
                | GapExtend              p   -> ["-gapextend"            ; string p]
                | Reward                 p   -> ["-reward"               ; string p]
                | Penalty                p   -> ["-penalty"              ; string p]
                | Strand                 p   -> ["-strand"               ; p]
                | Dust                   p   -> ["-dust"                 ; p]
                | FilteringDB            path-> ["-filtering_db"         ; (MountInfo.containerPathOf m path)]
                | WindowMaskerTaxId      p   -> ["-window_masker_taxid"  ; string p]
                | WindowMaskerDB         path-> ["-window_masker_db"     ; (MountInfo.containerPathOf m path)]
                | SoftMasking            p   -> ["-soft_masking"         ; string p]
                | LowerCaseMasking           -> ["-lcase_masking"]
                | DBSoftMask             p   -> ["-db_soft_mask"         ; string p]
                | DBHardMask             p   -> ["-db_hard_mask"         ; string p]
                | PercIdentity           p   -> ["-perc_identity"        ; string p]
                | XDropUngap             p   -> ["-xdrop_ungap"          ; string p]
                | XDropGap               p   -> ["-xdrop_gap"            ; string p]
                | XDropGapFinal          p   -> ["-xdrop_gap_final"      ; string p]
                | MinRawGappedScore      p   -> ["-min_raw_gapped_score" ; string p]
                | Ungapped                   -> ["-ungapped"]


        ///use this type to specify specific and generic command line parameters for the blastn megablast task like this:
        ///
        ///let myParams = [
        ///
        ///     MegablastParameters.CommonOptions [...]
        ///
        ///     MegablastParameters.SpecificOptions [...]
        ///]
        ///
        ///
        type MegablastParameters =  
            | CommonOptions     of BlastParams     list
            | SpecificOptions   of MegablastParams list

            static member makeCmd = function
                | CommonOptions     l -> l |> List.map BlastParams.makeCmd     |> List.concat
                | SpecificOptions   l -> l |> List.map MegablastParams.makeCmd |> List.concat

            static member makeCmdWith (m:MountInfo) = function
                | CommonOptions     l -> l |> List.map (BlastParams.makeCmdWith m)     |> List.concat
                | SpecificOptions   l -> l |> List.map (MegablastParams.makeCmdWith m) |> List.concat
    
        ///returns an asynchronous computation that will run `blastn -task megablast` in the specified container context with the given commands
        let runMegablastAsync (bcContext:BioContainer.BcContext) (opt:MegablastParameters list) = 
            let cmds = (opt |> List.map (MegablastParameters.makeCmdWith bcContext.Mount))
            let tp = "blastn"::"-task"::"megablast"::(cmds |> List.concat)

            printfn "Starting process megablast\r\nparameters:"
            cmds |> List.iter (fun op -> printfn "\t%s" (String.concat " " op))

            async {
                    let! res = BioContainer.execAsync bcContext tp           
                    return res
 
            }

        ///run `blastn -task megablast` in the specified container context with the given commands
        let runMegablast (bcContext:BioContainer.BcContext) (opt:MegablastParameters list) =
            runMegablastAsync bcContext opt
            |> Async.RunSynchronously

        ///use this type to specify specific and generic command line parameters for the blastn dc-megablast task like this:
        ///
        ///let myParams = [
        ///
        ///     DCMegablastParameters.CommonOptions [...]
        ///
        ///     DCMegablastParameters.SpecificOptions [...]
        ///]
        ///
        ///
        type DCMegablastParameters =  
            | CommonOptions     of BlastParams       list
            | SpecificOptions   of DCMegablastParams list

            static member makeCmd = function
                | CommonOptions     l -> l |> List.map BlastParams.makeCmd     |> List.concat
                | SpecificOptions   l -> l |> List.map DCMegablastParams.makeCmd |> List.concat

            static member makeCmdWith (m:MountInfo) = function
                | CommonOptions     l -> l |> List.map (BlastParams.makeCmdWith m)     |> List.concat
                | SpecificOptions   l -> l |> List.map (DCMegablastParams.makeCmdWith m) |> List.concat

        ///returns an asynchronous computation that will run `blastn -task dc-megablast` in the specified container context with the given commands
        let runDCMegablastNAsync (bcContext:BioContainer.BcContext) (opt:DCMegablastParameters list) = 
            let cmds = (opt |> List.map (DCMegablastParameters.makeCmdWith bcContext.Mount))
            let tp = "blastn"::"-task"::"dc-megablast"::(cmds |> List.concat)

            printfn "Starting process dc-megablast\r\nparameters:"
            cmds |> List.iter (fun op -> printfn "\t%s" (String.concat " " op))

            async {
                    let! res = BioContainer.execAsync bcContext tp           
                    return res
 
            }
        ///run `blastn -task dc-megablast` in the specified container context with the given commands
        let runDCMegablastN (bcContext:BioContainer.BcContext) (opt:DCMegablastParameters list) =
            runDCMegablastNAsync bcContext opt
            |> Async.RunSynchronously

        ///use this type to specify specific and generic command line parameters for the blastn task like this:
        ///
        ///let myParams = [
        ///
        ///     BlastNParameters.CommonOptions [...]
        ///
        ///     BlastNParameters.SpecificOptions [...]
        ///]
        ///
        ///
        type BlastNParameters =  
            | CommonOptions     of BlastParams  list
            | SpecificOptions   of BlastNParams list

            static member makeCmd = function
                | CommonOptions     l -> l |> List.map BlastParams.makeCmd     |> List.concat
                | SpecificOptions   l -> l |> List.map BlastNParams.makeCmd |> List.concat

            static member makeCmdWith (m:MountInfo) = function
                | CommonOptions     l -> l |> List.map (BlastParams.makeCmdWith m)     |> List.concat
                | SpecificOptions   l -> l |> List.map (BlastNParams.makeCmdWith m) |> List.concat
                
        ///returns an asynchronous computation that will run `blastn -task blastn` in the specified container context with the given commands
        let runBlastNAsync (bcContext:BioContainer.BcContext) (opt:BlastNParameters list) = 
            let cmds = (opt |> List.map (BlastNParameters.makeCmdWith bcContext.Mount))
            let tp = "blastn"::"-task"::"blastn"::(cmds |> List.concat)

            printfn "Starting process blastn\r\nparameters:"
            cmds |> List.iter (fun op -> printfn "\t%s" (String.concat " " op))

            async {
                    let! res = BioContainer.execAsync bcContext tp           
                    return res
 
            }

        ///run `blastn -task blastn` in the specified container context with the given commands
        let runBlastN (bcContext:BioContainer.BcContext) (opt:BlastNParameters list) =
            runBlastNAsync bcContext opt
            |> Async.RunSynchronously

        ///use this type to specify specific and generic command line parameters for the blastn-short task like this:
        ///
        ///let myParams = [
        ///
        ///     BlastNShortParameters.CommonOptions [...]
        ///
        ///     BlastNShortParameters.SpecificOptions [...]
        ///]
        ///
        ///
        type BlastNShortParameters =  
            | CommonOptions     of BlastParams           list
            | SpecificOptions   of BlastNShortParameters list

            static member makeCmd = function
                | CommonOptions     l -> l |> List.map BlastParams.makeCmd     |> List.concat
                | SpecificOptions   l -> l |> List.map BlastNShortParameters.makeCmd |> List.concat

            static member makeCmdWith (m:MountInfo) = function
                | CommonOptions     l -> l |> List.map (BlastParams.makeCmdWith m)     |> List.concat
                | SpecificOptions   l -> l |> List.map (BlastNShortParameters.makeCmdWith m) |> List.concat
                                                         
        ///returns an asynchronous computation that will run `blastn -task blastn-short` in the specified container context with the given commands
        let runBlastNShortAsync (bcContext:BioContainer.BcContext) (opt:BlastNShortParameters list) = 
            let cmds = (opt |> List.map (BlastNShortParameters.makeCmdWith bcContext.Mount))
            let tp = "blastn"::"-task"::"blastn-short"::(cmds |> List.concat)

            printfn "Starting process blastn-short\r\nparameters:"
            cmds |> List.iter (fun op -> printfn "\t%s" (String.concat " " op))

            async {
                    let! res = BioContainer.execAsync bcContext tp           
                    return res
 
            }
        ///run `blastn -task blastn-short` in the specified container context with the given commands
        let runBlastNShort (bcContext:BioContainer.BcContext) (opt:BlastNShortParameters list) =
            runBlastNShortAsync bcContext opt
            |> Async.RunSynchronously
    
    ///DSL for blastp programs
    ///The blastp application searches a protein sequence against protein subject sequences or a protein database. 
    ///Three different tasks are supported: 
    ///
    ///1.) “blastp”, for standard protein-protein comparisons 
    ///
    ///2.) “blastp-short”, optimized for query sequences shorter than 30 residues 
    ///
    ///3.) “blastp-fast”, a faster version that uses a larger word-size
    [<RequireQualifiedAccess>]
    module BlastP =
        
        ///DSL fo the blastp 'blastp' task command line options
        ///blastp is used for standard protein-protein comparisons 
        type BlastPParams =
            ///Word size of initial match. Valid word sizes are 2-7.
            | WordSize          of int
            ///Cost to open a gap.
            | GapOpen           of int
            ///Cost to extend a gap.
            | GapExtend         of int
            ///Scoring matrix name.
            | Matrix            of string
            ///Minimum score to add a word to the BLAST lookup table.
            | Threshold         of int
            ///Use composition-based statistics: D or d: default (equivalent to 2) 0 or F or f: no composition-based statistics 21:902-911, 2005, conditioned on sequence properties3: Composition-based score adjustment as in Bioinformatics 21:902-911, 2005, unconditionally
            //TODO: refactor into proper type
            | CompBasedStats    of string
            ///Apply filtering locations as soft masks (i.e., only for finding initial matches).
            | SoftMasking       of bool
            ///Multiple hits window size, use 0 to specify 1-hit algorithm.
            | WindowSize        of int
            ///Filter query sequence with SEG (Format: 'yes', 'window locut hicut', or 'no' to disable).
            //TODO: refactor into proper type
            | Seg               of string
            ///Use lower case filtering in query and subject sequence(s).
            | LowerCaseMasking
            ///db_soft_mask     all    of int       none    Filtering algorithm ID to apply to the BLAST database as soft mask (i.e., only for finding initial matches).
            | DBSoftMask        of int
            ///db_hard_mask     all    of int       none    Filtering algorithm ID to apply to the BLAST database as hard mask (i.e., sequence is masked for all phases of search).
            | DBHardMask        of int       
            ///xdrop_gap_final  all    of float     25    Heuristic value (in bits) for final gapped alignment/
            | XDropGapFinal     of float
            ///use_sw_tback     all                 N/A    Compute locally optimal Smith-Waterman alignments?
            | UseSWTback    
        
            static member makeCmd = function
                | Seg                p -> ["-seg"               ; string p]
                | LowerCaseMasking     -> ["-lcase_masking"]
                | DBSoftMask         p -> ["-db_soft_mask"      ; string p]
                | DBHardMask         p -> ["-db_hard_mask"      ; string p]
                | XDropGapFinal      p -> ["-xdrop_gap_final"   ; string p]
                | UseSWTback           -> ["-use_sw_tback"]            
                | WordSize           p -> ["-word_size"         ; string p]
                | GapOpen            p -> ["-gapopen"           ; string p]
                | GapExtend          p -> ["-gapextend"         ; string p]
                | Matrix             p -> ["-matrix"            ; string p]
                | Threshold          p -> ["-threshold"         ; string p]
                | CompBasedStats     p -> ["-comp_based_stats"  ; string p]
                | SoftMasking        p -> ["-soft_masking"      ; string p]
                | WindowSize         p -> ["-window_size"       ; string p]

            static member makeCmdWith (m:MountInfo) = function
                | Seg                p -> ["-seg"               ; string p]
                | LowerCaseMasking     -> ["-lcase_masking"]
                | DBSoftMask         p -> ["-db_soft_mask"      ; string p]
                | DBHardMask         p -> ["-db_hard_mask"      ; string p]
                | XDropGapFinal      p -> ["-xdrop_gap_final"   ; string p]
                | UseSWTback           -> ["-use_sw_tback"]
                | WordSize           p -> ["-word_size"         ; string p]
                | GapOpen            p -> ["-gapopen"           ; string p]
                | GapExtend          p -> ["-gapextend"         ; string p]
                | Matrix             p -> ["-matrix"            ; string p]
                | Threshold          p -> ["-threshold"         ; string p]
                | CompBasedStats     p -> ["-comp_based_stats"  ; string p]
                | SoftMasking        p -> ["-soft_masking"      ; string p]
                | WindowSize         p -> ["-window_size"       ; string p]

        ///use this type to specify specific and generic command line parameters for the blastp task like this:
        ///
        ///let myParams = [
        ///
        ///     BlastPParameters.CommonOptions [...]
        ///
        ///     BlastPParameters.SpecificOptions [...]
        ///]
        type BlastPParameters =
            | CommonOptions     of BlastParams      list
            | SpecificOptions   of BlastPParams     list

            static member makeCmd = function
                | CommonOptions     l -> l |> List.map BlastParams.makeCmd     |> List.concat
                | SpecificOptions   l -> l |> List.map BlastPParams.makeCmd |> List.concat

            static member makeCmdWith (m:MountInfo) = function
                | CommonOptions     l -> l |> List.map (BlastParams.makeCmdWith m)     |> List.concat
                | SpecificOptions   l -> l |> List.map (BlastPParams.makeCmdWith m) |> List.concat
        
        ///returns an asynchronous computation that will run `blastp -task blastp` in the specified container context with the given commands
        let runBlastPAsync (bcContext:BioContainer.BcContext) (opt:BlastPParameters list) = 
            let cmds = (opt |> List.map (BlastPParameters.makeCmdWith bcContext.Mount))
            let tp = "blastp"::"-task"::"blastp"::(cmds |> List.concat)

            printfn "Starting process blastp\r\nparameters:"
            cmds |> List.iter (fun op -> printfn "\t%s" (String.concat " " op))

            async {
                    let! res = BioContainer.execAsync bcContext tp           
                    return res
            }

        ///run `blastn -task blastn-short` in the specified container context with the given commands
        let runBlastP (bcContext:BioContainer.BcContext) (opt:BlastPParameters list) = 
            runBlastPAsync bcContext opt
            |> Async.RunSynchronously

        ///DSL fo the blastp 'blastp-short' task command line options
        ///blastp-short is optimized for query sequences shorter than 30 residues 
        type BlastPShortParams =
            ///Word size of initial match.
            | WordSize of int 
            ///Cost to open a gap.
            | GapOpen    of int 
            ///Cost to extend a gap.
            | GapExtend    of int  
            ///Scoring matrix name.
            | Matrix of string
            ///thresholdMinimum score to add a word to the BLAST lookup table.
            | Threshold    of int
            ///Use composition-based statistics : D or d: default (equivalent to 2) 0 or F or f: no composition-based statistics 1: Composition-based statistics as in NAR 29:2994-3005, 2001 2 or T or t : Composition-based score adjustment as in Bioinformatics 21:902-911, 2005, conditioned on sequence properties 3: Composition-based score adjustment as in Bioinformatics 21:902-911, 2005, unconditionally
            //TODO: refactor into proper type
            | CompBasedStats of string
            ///Multiple hits window size, use 0 to specify 1-hit algorithm.
            | WindowSize    of int
            ///Filter query sequence with SEG (Format: 'yes', 'window locut hicut', or 'no' to disable).
            //TODO: refactor into proper type
            | Seg               of string
            ///Use lower case filtering in query and subject sequence(s).
            | LowerCaseMasking
            ///Filtering algorithm ID to apply to the BLAST database as soft mask (i.e., only for finding initial matches).
            | DBSoftMask        of int
            ///Filtering algorithm ID to apply to the BLAST database as hard mask (i.e., sequence is masked for all phases of search).
            | DBHardMask        of int       
            ///Heuristic value (in bits) for final gapped alignment/
            | XDropGapFinal     of float
            ///Compute locally optimal Smith-Waterman alignments?
            | UseSWTback    
        
            static member makeCmd = function
                | Seg                p -> ["-seg"            ; string p]
                | LowerCaseMasking     -> ["-lcase_masking"]
                | DBSoftMask         p -> ["-db_soft_mask"   ; string p]
                | DBHardMask         p -> ["-db_hard_mask"   ; string p]
                | XDropGapFinal      p -> ["-xdrop_gap_final"; string p]
                | UseSWTback           -> ["-use_sw_tback"]            
                | WordSize           p -> ["-word_size"]       
                | GapOpen            p -> ["-gapopen"         ;]
                | GapExtend          p -> ["-gapextend"       ;]
                | Matrix             p -> ["-matrix"          ;]
                | Threshold          p -> ["-threshold"       ;]
                | CompBasedStats     p -> ["-comp_based_stats";]
                | WindowSize         p -> ["-window_size"     ;]

            static member makeCmdWith (m:MountInfo) = function
                | Seg                p -> ["-seg"            ; string p]
                | LowerCaseMasking     -> ["-lcase_masking"]
                | DBSoftMask         p -> ["-db_soft_mask"   ; string p]
                | DBHardMask         p -> ["-db_hard_mask"   ; string p]
                | XDropGapFinal      p -> ["-xdrop_gap_final"; string p]
                | UseSWTback           -> ["-use_sw_tback"]
                | WordSize           p -> ["-word_size"]       
                | GapOpen            p -> ["-gapopen"         ;]
                | GapExtend          p -> ["-gapextend"       ;]
                | Matrix             p -> ["-matrix"          ;]
                | Threshold          p -> ["-threshold"       ;]
                | CompBasedStats     p -> ["-comp_based_stats";]
                | WindowSize         p -> ["-window_size"     ;]

        ///use this type to specify specific and generic command line parameters for the blastp blastp-short task like this:
        ///
        ///let myParams = [
        ///
        ///     BlastPShortParameters.CommonOptions [...]
        ///
        ///     BlastPShortParameters.SpecificOptions [...]
        ///]
        ///
        ///
        type BlastPShortParameters =
            | CommonOptions     of BlastParams          list
            | SpecificOptions   of BlastPShortParams    list

            static member makeCmd = function
                | CommonOptions     l -> l |> List.map BlastParams.makeCmd     |> List.concat
                | SpecificOptions   l -> l |> List.map BlastPShortParams.makeCmd |> List.concat

            static member makeCmdWith (m:MountInfo) = function
                | CommonOptions     l -> l |> List.map (BlastParams.makeCmdWith m)     |> List.concat
                | SpecificOptions   l -> l |> List.map (BlastPShortParams.makeCmdWith m) |> List.concat
        
        ///returns an asynchronous computation that will run `blastp -task blastp-short` in the specified container context with the given commands
        let runBlastPShortAsync (bcContext:BioContainer.BcContext) (opt:BlastPShortParameters list) = 
            let cmds = (opt |> List.map (BlastPShortParameters.makeCmdWith bcContext.Mount))
            let tp = "blastp"::"-task"::"blastp-short"::(cmds |> List.concat)

            printfn "Starting process blastp-short\r\nparameters:"
            cmds |> List.iter (fun op -> printfn "\t%s" (String.concat " " op))

            async {
                    let! res = BioContainer.execAsync bcContext tp           
                    return res
            }

        ///returns an asynchronous computation that will run `blastp -task blastp-short` in the specified container context with the given commands
        let runBlastPShort (bcContext:BioContainer.BcContext) (opt:BlastPShortParameters list) = 
            runBlastPShortAsync bcContext opt
            |> Async.RunSynchronously

        ///DSL fo the blastp 'blastp-fast' task command line options
        ///blastp-fast is a faster version that uses a larger word-size
        type BlastPFastParams =
            ///Word size of initial match
            | WordSize          of int
            ///Minimum score to add a word to the BLAST lookup table.
            | Threshold         of int
            ///Use composition-based statistics:D or d: default (equivalent to 2) 0 or F or f: no composition-based statistics
            //TODO: refactor into proper type
            |CompBasedStats     of string  
            ///Multiple hits window size, use 0 to specify 1-hit algorithm.
            | WindowSize        of int
            ///Filter query sequence with SEG (Format: 'yes', 'window locut hicut', or 'no' to disable).
            //TODO: refactor into proper type
            | Seg               of string
            ///Use lower case filtering in query and subject sequence(s).
            | LowerCaseMasking
            ///db_soft_mask     all    of int       none    Filtering algorithm ID to apply to the BLAST database as soft mask (i.e., only for finding initial matches).
            | DBSoftMask        of int
            ///db_hard_mask     all    of int       none    Filtering algorithm ID to apply to the BLAST database as hard mask (i.e., sequence is masked for all phases of search).
            | DBHardMask        of int       
            ///xdrop_gap_final  all    of float     25    Heuristic value (in bits) for final gapped alignment/
            | XDropGapFinal     of float
            ///use_sw_tback     all                 N/A    Compute locally optimal Smith-Waterman alignments?
            | UseSWTback    
        
            static member makeCmd = function
                | Seg                p -> ["-seg"            ; string p]
                | LowerCaseMasking     -> ["-lcase_masking"]
                | DBSoftMask         p -> ["-db_soft_mask"   ; string p]
                | DBHardMask         p -> ["-db_hard_mask"   ; string p]
                | XDropGapFinal      p -> ["-xdrop_gap_final"; string p]
                | UseSWTback           -> ["-use_sw_tback"]            
                | WordSize           p -> ["-word size"       ; string p]
                | Threshold          p -> ["-Threshold"       ; string p]
                | CompBasedStats     p -> ["-comp_based_stats"; string p]
                | WindowSize         p -> ["-window_size"     ; string p]

            static member makeCmdWith (m:MountInfo) = function
                | Seg                p -> ["-seg"            ; string p]
                | LowerCaseMasking     -> ["-lcase_masking"]
                | DBSoftMask         p -> ["-db_soft_mask"   ; string p]
                | DBHardMask         p -> ["-db_hard_mask"   ; string p]
                | XDropGapFinal      p -> ["-xdrop_gap_final"; string p]
                | UseSWTback           -> ["-use_sw_tback"]
                | WordSize           p -> ["-word size"       ; string p]
                | Threshold          p -> ["-Threshold"       ; string p]
                | CompBasedStats     p -> ["-comp_based_stats"; string p]
                | WindowSize         p -> ["-window_size"     ; string p]

        ///use this type to specify specific and generic command line parameters for the blastp blastp-fast task like this:
        ///
        ///let myParams = [
        ///
        ///     BlastPFastParameters.CommonOptions [...]
        ///
        ///     BlastPFastParameters.SpecificOptions [...]
        ///]
        ///
        ///
        type BlastPFastParameters =
            | CommonOptions     of BlastParams          list
            | SpecificOptions   of BlastPFastParams    list

            static member makeCmd = function
                | CommonOptions     l -> l |> List.map BlastParams.makeCmd     |> List.concat
                | SpecificOptions   l -> l |> List.map BlastPFastParams.makeCmd |> List.concat

            static member makeCmdWith (m:MountInfo) = function
                | CommonOptions     l -> l |> List.map (BlastParams.makeCmdWith m)     |> List.concat
                | SpecificOptions   l -> l |> List.map (BlastPFastParams.makeCmdWith m) |> List.concat

        ///returns an asynchronous computation that will run `blastp -task blastp-fast` in the specified container context with the given commands
        let runBlastPFastAsync (bcContext:BioContainer.BcContext) (opt:BlastPFastParameters list) = 
            let cmds = (opt |> List.map (BlastPFastParameters.makeCmdWith bcContext.Mount))
            let tp = "blastp"::"-task"::"blastp-fast"::(cmds |> List.concat)

            printfn "Starting process blastp\r\nparameters:"
            cmds |> List.iter (fun op -> printfn "\t%s" (String.concat " " op))

            async {
                    let! res = BioContainer.execAsync bcContext tp           
                    return res
            }

        ///run `blastp -task blastp-short` in the specified container context with the given commands
        let runBlastPFast (bcContext:BioContainer.BcContext) (opt:BlastPFastParameters list) = 
            runBlastPFastAsync bcContext opt
            |> Async.RunSynchronously