namespace BioFSharp.IO

module BlastNCBI =

    open System
    open System.Diagnostics
    
    module Parameters =

        type DbType =
        | Protein 
        | Nucleotide

        let private stringOfDbType (param:DbType) =
            match param with
            | Protein -> "prot"
            | Nucleotide -> "nucl"

        type MakeDbParams =
            | Input  of string
            //| Output of String
            | DbType of DbType
            | MaskData of string
            | ParseSeqIds    


        let stringOfMakeDbParams (param:MakeDbParams) =
            match param with
            | Input  (path)   -> sprintf "-in %s -out %s"  path path
            //| Output (path) -> sprintf "-out %s" path
            | DbType (dbt)    -> sprintf "-dbtype %s" (stringOfDbType dbt)
            | MaskData (path) -> sprintf "-mask_data %s.asnb" path 
            | ParseSeqIds     -> sprintf "-parse_seqids" 



        type OutputType = 
            | Pairwise                          =  0
            | Query_anchored                    =  1
            | Query_anchored_NoIdentities       =  2
            | Query_anchored_Flat               =  3
            | Query_anchored_Flat_NoIdentities  =  4  
            | XML                               =  5
            | Tabular                           =  6
            | TabularWithComments               =  7
            | TextASN1                          =  8
            | BinaryASN1                        =  9
            | CSV                               = 10
            | BLAST_ArchiveFormat               = 11
            | JSON_Seqalign                     = 12
            | JSON_Blast                        = 13
            | XML2_Blast                        = 14

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



        let myDefault_OutputCustom = 
            [   OutputCustom.Query_SeqId;OutputCustom.Subject_SeqId;OutputCustom.Query_Length;
                OutputCustom.Subject_Length;OutputCustom.AlignmentLength;OutputCustom.MismatchCount;
                OutputCustom.IdentityCount;OutputCustom.PositiveScoringMatchCount;OutputCustom.Evalue;
                OutputCustom.Bitscore;
            ]
                          




        let stringOfOutputCustom (param:OutputCustom) =
            match param with
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




   

 

        // http://www.ncbi.nlm.nih.gov/books/NBK279675/
        type BlastParams =
            | SearchDB of string
            | Query    of string
            | Output   of string
            | OutputType of OutputType
            | OutputTypeCustom of OutputType * seq<OutputCustom>
            | Num_threads of int
            | Max_Hits of int

        let stringOfBlastParams (param:BlastParams) =
            match param with
            | SearchDB  (path)      -> sprintf "-db %s"    path
            | Query     (path)      -> sprintf "-query %s" path
            | Output    (path)      -> sprintf "-out %s"   path
            | OutputType(format)    -> sprintf "-outfmt %i" (format |> int)
            | OutputTypeCustom(t,p) -> let tmp = p |> Seq.map stringOfOutputCustom |> String.concat " "
                                       match t with
                                       | OutputType.Tabular             -> sprintf "-outfmt \"%i %s\"" (t |> int) tmp
                                       | OutputType.TabularWithComments -> sprintf "-outfmt %i %s" (t |> int) tmp
                                       | OutputType.CSV                 -> sprintf "-outfmt %i %s" (t |> int) tmp
                                       | _ -> failwithf "Output format %A does not support custom columns." t                                
            | Num_threads(i)        -> sprintf "-num_threads %i" i
            | Max_Hits (i)          -> sprintf "-max_target_seqs %i" i
            //| evalue







    open Parameters
    open FSharp.Care.IO
    open FSharp.Care.IO.SchemaReader
    open FSharp.Care.IO.SchemaReader.Attribute


    let ncbiPath = "../../lib/ncbi-blast/bin"

    ///A Wrapper to perform different BLAST tasks
    type BlastWrapper (rootPath:string) =
        let createArguments (f : 'a -> string) (ps:seq<'a>) =
            ps |> Seq.map f
            |> String.concat " "
        
        let createProcess name exec (f : 'TParam -> string) (ps:seq<'TParam>) =
            let arg = createArguments f ps
            let beginTime = DateTime.UtcNow
            printfn "Starting %s..." name
            let p =                        
                new ProcessStartInfo
                  (FileName = rootPath + exec, UseShellExecute = false, Arguments = arg, 
                   RedirectStandardError = false, CreateNoWindow = true, 
                   RedirectStandardOutput = false, RedirectStandardInput = true) 
                |> Process.Start
            p.WaitForExit()
            printfn "%s done." name
            printfn "Elapsed time: %A" (beginTime.Subtract(DateTime.UtcNow))

        ///Create a BLAST database from given source
        member this.makeblastdb searchDB (ps:seq<Parameters.MakeDbParams>) = 
            let arg = [Parameters.MakeDbParams.Input searchDB;]
            createProcess "Makeblastdb" "/makeblastdb.exe" Parameters.stringOfMakeDbParams (Seq.append arg ps)       
        
        ///Perform a protein BLAST
        member this.blastP searchDB query output  (ps:seq<Parameters.BlastParams>) = 
            let arg = [Parameters.BlastParams.SearchDB searchDB; Parameters.BlastParams.Query query; Parameters.BlastParams.Output output]
            createProcess "BlastP" "/blastp.exe" Parameters.stringOfBlastParams (Seq.append arg ps)

        ///Perform a nucleotide BLAST
        member this.blastN searchDB query output (ps:seq<Parameters.BlastParams>) = 
            let arg = [Parameters.BlastParams.SearchDB searchDB; Parameters.BlastParams.Query query; Parameters.BlastParams.Output output]
            createProcess "BlastN" "/blastn.exe" Parameters.stringOfBlastParams (Seq.append arg ps)





    
    type CBlastResult = {

       [<FieldAttribute("qseqid")>]      Query_SeqId               : string
       [<FieldAttribute("qgi")>]         Query_GI                  : string
       [<FieldAttribute("qacc")>]        Query_Accesion            : string
       [<FieldAttribute("qaccver")>]     Query_Accesion_Version    : string
       [<FieldAttribute("qlen")>]        Query_Length              : int
       [<FieldAttribute("sseqid")>]      Subject_SeqId             : string
       [<FieldAttribute("sallseqid")>]   Subject_All_SeqIds        : string
       [<FieldAttribute("sgi")>]         Subject_GI                : string
       [<FieldAttribute("sallgi")>]      Subject_All_GIs           : string
       [<FieldAttribute("sacc")>]        Subject_Accession         : string
       [<FieldAttribute("saccver")>]     Subject_Accession_Version : string
       [<FieldAttribute("sallacc")>]     Subject_All_Accession     : string
       [<FieldAttribute("slen")>]        Subject_Length            : int
       [<FieldAttribute("qstart")>]      Query_StartOfAlignment    : string
       [<FieldAttribute("qend")>]        Query_EndOfAlignment      : string
       [<FieldAttribute("sstart")>]      Subject_StartOfAlignment  : string
       [<FieldAttribute("send")>]        Subject_EndOfAlignment    : string
       [<FieldAttribute("qseq")>]        Query_AlignedPartOf       : string
       [<FieldAttribute("sseq" )>]       Subject_AlignedPartOf     : string
       [<FieldAttribute("evalue")>]      Evalue                    : float
       [<FieldAttribute("bitscore")>]    Bitscore                  : float
       [<FieldAttribute("score")>]       RawScore                  : float
       [<FieldAttribute("length")>]      AlignmentLength           : int
       [<FieldAttribute("pident" )>]     Identity                  : float
       [<FieldAttribute("nident")>]      IdentityCount             : int
       [<FieldAttribute("mismatch")>]    MismatchCount             : int
       [<FieldAttribute("positive")>]    PositiveScoringMatchCount : int
       [<FieldAttribute("gapopen")>]     GapOpeningCount           : int
       [<FieldAttribute("gaps")>]        GapCount                  : int
       [<FieldAttribute("ppos")>]        PositiveScoringMatch      : float
                                        
       [<FieldAttribute("frames" )>]     Frames                    : string
       [<FieldAttribute("qframe")>]      Query_Frames              : string
       [<FieldAttribute("sframe")>]      Subject_Frames            : string
                 
       [<FieldAttribute("btop" )>]       BTOP                      : string
       [<FieldAttribute("staxids" )>]    Subject_TaxonomyIDs       : string
       [<FieldAttribute("sscinames")>]   Subject_Scientific_Names  : string
       [<FieldAttribute("scomnames")>]   Subject_Common_Names      : string
       [<FieldAttribute("sblastnames")>] Subject_Blast_Names       : string
       [<FieldAttribute("sskingdoms")>]  Subject_Super_Kingdoms    : string
       [<FieldAttribute("stitle")>]      Subject_Title             : string
       [<FieldAttribute("salltitles")>]  Subject_All_Titles        : string
       [<FieldAttribute("sstrand")>]     Subject_Strand            : string
       [<FieldAttribute("qcovs")>]       Query_CoveragePerSubject  : string
       [<FieldAttribute("qcovhsp")>]     Query_CoveragePerHSP      : string

       }





    let readCustomBlastResult (cAttributes:seq<OutputCustom>) separator filePath =
        let lineHasHeader = cAttributes |> Seq.map stringOfOutputCustom |> String.concat (separator.ToString())
        let csvReader = SchemaReader.Csv.CsvReader<CBlastResult>(schemaMode=SchemaReader.Csv.Fill)
        csvReader.ReadFile(filePath,separator,lineHasHeader)

