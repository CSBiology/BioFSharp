namespace BioFSharp.BioTools

module Blast =

    open FSharpAux
    open FSharpAux.IO
    open FSharpAux.IO.SchemaReader.Attribute
    open BioContainer
    open BioContainerIO
     
    type DbType =
        | Protein 
        | Nucleotide

        static member make = function
            | Protein       -> "prot"
            | Nucleotide    -> "nucl"


    type MakeDbParams =
        | Input  of string
        | Output of string
        | DbType of DbType
        | MaskData of string
        | ParseSeqIds    

        static member makeCmdWith (m: MountInfo) = function
            | Input  (path)     -> ["-in"  ;(MountInfo.containerPathOf m path)]
            | Output (path)     -> ["-out" ;(MountInfo.containerPathOf m path)]
            | DbType (dbt)      -> ["-dbtype"; (DbType.make dbt)]
            | MaskData (path)   -> ["-mask_data"; (sprintf "%s.asnb") (MountInfo.containerPathOf m path)]
            | ParseSeqIds       -> ["-parse_seqids"] 


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


    let runMakeBlastDBAsync (bcContext:BioContainer.BcContext) (opt:MakeDbParams list) = 

        let cmds = (opt |> List.map (MakeDbParams.makeCmdWith bcContext.Mount))
        let tp = "makeblastdb"::(cmds |> List.concat)

        printfn "Starting process makeblastdb\r\nparameters:"
        cmds |> List.iter (fun op -> printfn "\t%s" (String.concat " " op))

        async {
                let! res = BioContainer.execAsync bcContext tp           
                return res
        }

    let runMakeBlastDB (bcContext:BioContainer.BcContext) (opt:MakeDbParams list) =

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