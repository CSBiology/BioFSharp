namespace BioFSharp.IO

///Wrapper and its helpers for Clustal Omega multiple alignment tools
module ClustalOWrapper =

    ///Contains modifier parameter type for Clustal Omega wrapper
    module Parameters = 
    
        ///Input file format
        type FileFormat = 
            ///FastA file format
            | FastA
            ///Clustal file format
            | Clustal
            ///MSF file format
            | MSF
            ///Phylip file format
            | Phylip
            ///Selex file format
            | Selex
            ///Stockholm file format
            | Stockholm
            ///Vienna file format
            | Vienna
    
        let private stringOfFileFormatOut (f:FileFormat) =
            match f with
            | FastA -> "--outfmt=fa "
            | Clustal -> "--outfmt=clu "
            | MSF -> "--outfmt=msf "
            | Phylip -> "--outfmt=phy "
            | Selex -> "--outfmt=selex "
            | Stockholm -> "--outfmt=st "
            | Vienna -> "--outfmt=vie "

        let private stringOfFileFormatIn (f:FileFormat) =
            match f with
            | FastA -> "--infmt=fa "
            | Clustal -> "--infmt=clu "
            | MSF -> "--infmt=msf "
            | Phylip -> "--infmt=phy "
            | Selex -> "--infmt=selex "
            | Stockholm -> "--infmt=st "
            | Vienna -> "--infmt=vie "
        ///Types of sequences
        type SeqType = 
            | Protein
            | DNA
            | RNA

        let private stringOfSeqType (s:SeqType) = 
            match s with
            | Protein -> "--seqtype=Protein "
            | RNA -> "--seqtype=RNA "
            | DNA -> "--seqtype=DNA "

        ///Optional modifiers for input
        type InputCustom =
            ///Forced sequence input file format (default: auto)
            | Format of FileFormat
            ///Dealign input sequences
            | Dealign
            ///Disable check if profile, force profile (default no)
            | IsProfile
            ///Force a sequence type (default: auto)
            | SeqType of SeqType    
    
        let private stringOfInputCustom (i:InputCustom) = 
            match i with
            | Format f -> stringOfFileFormatIn f
            | Dealign -> "--dealign "
            | IsProfile -> "--is-profile "
            | SeqType s -> stringOfSeqType s

        ///Optional modifiers to specify clustering
        type ClusteringCustom =
            ///Pairwise distance matrix input file (skips distance computation)
            | DistanceMatrixInput of string
            ///Pairwise distance matrix output file
            | DistanceMatrixOutput of string
            ///Guide tree input file (skips distance computation and guide tree clustering step)
            | GuideTreeInput of string
            ///Guide tree output file
            | GuideTreeOutput of string
            ///Use full distance matrix for guide-tree calculation (slow; mBed is default)
            | Full
            ///Use full distance matrix for guide-tree calculation during iteration (mBed is default)
            | FullIter
            /// Soft maximum of sequences in sub-clusters
            | ClusterSize of int
            ///	Clustering output file
            | ClusteringOut of string
            /// Use Kimura distance correction for aligned sequences (default no)
            | UseKimura
            /// convert distances into percent identities (default no)
            | PercentID
    
        let private stringOfClusteringCustom (c:ClusteringCustom) = 
            match c with
            | DistanceMatrixInput c -> sprintf "--distmat-in=%s " c
            | DistanceMatrixOutput c -> sprintf "--distmat-out=%s " c
            | GuideTreeInput c -> sprintf "--guidetree-in=%s " c
            | GuideTreeOutput c -> sprintf "--guidetree-out=%s " c
            | Full -> "--full "
            | FullIter -> "--full-iter "
            | ClusterSize i -> sprintf "--cluster-size=%i " i
            | ClusteringOut c -> sprintf "--clustering-out=%s " c
            | UseKimura -> "--use-kimura "
            | PercentID -> "--percent-id "

        ///Optional modifiers for input
        type OutputCustom =
            ///	MSA output file format (default: fasta)
            | Format of FileFormat
            ///	in Clustal format print residue numbers (default no)
            | ResidueNumber 
            ///	number of residues before line-wrap in output
            | Wrap of int
            /// Aligned sequences are ordered according to guide tree instead of input order
            | OutputOrderAsTree

        let private stringOfOutputCustom (o:OutputCustom) = 
            match o with
            | Format f -> stringOfFileFormatOut f
            | ResidueNumber -> "--residuenumber "
            | Wrap i -> sprintf "--wrap=%i " i
            | OutputOrderAsTree -> "--output-order=tree-order "
        
        ///Specify maximum number of iterations for given step
        type IterationCustom =
            /// Number of (combined guide tree/HMM) iterations
            | Iterations of int
            /// Maximum guide tree iterations
            | MaxGuideTreeIterations of int
            ///  Maximum number of HMM iterations
            | MaxHMMIterations of int
    
        let private stringOfIterationCustom (i:IterationCustom) =
            match i with
            | Iterations i -> sprintf "--iter=%i " i
            | MaxGuideTreeIterations i -> sprintf "--max-guidetree-iterations=%i " i
            | MaxHMMIterations i -> sprintf "--max-hmm-iterations=%i " i

    
        /// Will exit early, if exceeded
        type LimitsCustom =
            /// Maximum allowed number of sequences
            | MaxSeqNumber of int
            /// Maximum allowed sequence length
            | MaxSeqLength of int
    
        let private stringOfLimits (l:LimitsCustom) =
            match l with
            | MaxSeqNumber i -> sprintf "--maxnumseq=%i " i
            | MaxSeqLength i -> sprintf "--maxseqlen=%i " i

        ///Optional, miscallaneous modifiers 
        type MiscallaneousCustom =
            /// Set options automatically (might overwrite some of your options)
            | Auto
            /// Number of processors to use
            | Threads of int
            /// Log all non-essential output to this file
            | Log of string
            /// Print help and exit
            //| Help
            /// Verbose output (ranging from 0 [nonverbose,standard] to 3 [very verbose,everything above 3 is set to 3])
            | VerboseLevel of int
            /// Print version information and exit
            | Version
            /// Print long version information and exit
            | LongVersion
            /// Force file overwriting
            | Force
    
        let private stringOfMiscallaneous (m:MiscallaneousCustom) =
            match m with
            | Auto -> "--auto "
            | Threads i -> sprintf "--threads=%i " i
            | Log s -> sprintf "--log=%s " s
            //| Help -> "--help "
            | VerboseLevel i -> 
                let sb = System.Text.StringBuilder()
                let n = if i < 0 then 0; elif i > 3 then 3; else i
                let s = 
                    for i = 0 to (n-1) do 
                        sb.Append("-v ") |> ignore
                    sb.ToString()
                sb.Clear() |> ignore
                s
            | Version -> "--version "
            | LongVersion -> "--long-version "
            | Force -> "--force "

        ///Collection of parameters for specifying clustalo alignment
        type ClustalParams = 
            /// Specify input parameters
            | Input of seq<InputCustom>
            /// Specify output parameters
            | Output of seq<OutputCustom>
            /// Specify clustering parameters
            | Clustering of seq<ClusteringCustom>
            /// Specify iteration parameters
            | Iteration of seq<IterationCustom>
            /// Specify limits parameters
            | Limits of seq<LimitsCustom>
            /// Specify miscallaneous parameters
            | Miscallaneous of seq<MiscallaneousCustom>



        ///Create argument string for clustal parameter
        let stringOfClustalParams (c:ClustalParams) = 
            let iterCustom f s =
                Seq.map f s
                |> String.concat ""
            match c with
            | Input s -> iterCustom stringOfInputCustom s
            | Output s -> iterCustom stringOfOutputCustom s
            | Clustering s -> iterCustom stringOfClusteringCustom s
            | Iteration s -> iterCustom stringOfIterationCustom s
            | Limits s -> iterCustom stringOfLimits s
            | Miscallaneous s -> iterCustom stringOfMiscallaneous s

    open Parameters
    open BioFSharp.IO
    ///Specify the type of input and assign file path
    type Input = 
        ///Use this option to make a multiple alignment from a set of sequences. A sequence file must contain more than one sequence (at least two sequences).
        | SequenceFile of string 
        ///Use this option to align two alignments (profiles) together.
        | TwoProfiles of string * string 
        /// Use this option to add new sequences to an existing alignment.
        | SequenceFileAndProfile of string * string
        /// Use this option to make a new multiple alignment of sequences from the input file and use the HMM as a guide (EPA).
        | SequenceFileAndHMM of string * string
    
    let private stringOfInputType (i:Input) = 
        match i with
        | SequenceFile s -> sprintf "-i %s " s
        | TwoProfiles (s1,s2) -> sprintf "--p1 %s --p2 %s " s1 s2
        | SequenceFileAndProfile (s1,s2) -> sprintf "-i %s --p1 %s " s1 s2
        | SequenceFileAndHMM (s1,s2) -> sprintf "-i %s --hmm-in %s " s1 s2

    open System
    open System.Diagnostics
    open System.IO
    open BioFSharp.BioID
    
    let private tsToFasta (ts:TaggedSequence<string,char>) =
        {FastA.Header = ts.Tag;
        FastA.Sequence = ts.Sequence}
    
    /// A wrapper to perform Clustal Omega alignment tasks    
    type ClustalOWrapper (?rootPath: string) =
        
        //isLinux = System.Environment.OSVersion.Platform = System.PlatformID.Unix
        #if INTERACTIVE
        
        let rootPath = 
            match rootPath with
            | Some r -> 
                if File.Exists r then r
                else failwith "clustalo file could not be found for given rootPath"
            | None -> 
                let defaultPath = __SOURCE_DIRECTORY__ |> FSharp.Care.String.replace "src\BioFSharp.IO" @"lib\clustalomega'\clustalo.exe"
                printfn "try %s" defaultPath
                if File.Exists defaultPath then defaultPath
                else failwith "Default clustalo file could not be found, define rootPath argument."
        #else
        let rootPath = 
            match rootPath with
            | Some r -> 
                if File.Exists r then r
                else failwith "clustalo file could not be found for given rootPath"
            | None -> 
                let defaultPath = __SOURCE_DIRECTORY__ |> FSharp.Care.String.replace "src\BioFSharp.IO" @"lib\clustalomega'\clustalo.exe"
                printfn "try %s" defaultPath
                if File.Exists defaultPath then defaultPath
                else failwith "Default clustalo file could not be found, define rootPath argument."
        let runProcess rootPath arg name =           
            let beginTime = DateTime.UtcNow
            printfn "Starting %s..." name
            let p = 
                new ProcessStartInfo
                    (FileName = rootPath, UseShellExecute = false, Arguments = arg, 
                    RedirectStandardError = false, CreateNoWindow = true, 
                    RedirectStandardOutput = false, RedirectStandardInput = true) 
                |> Process.Start
            p.WaitForExit()
            printfn "%s done." name
            printfn "Elapsed time: %A" (beginTime.Subtract(DateTime.UtcNow))
        #endif
        ///Runs clustalo tool with given input file paths and parameters and creates output file for given path
        member this.AlignFromFile((inputPath:Input),(outputPath:string),(parameters:seq<Parameters.ClustalParams>),(?name:string)) = 
            let out = sprintf "-o %s " outputPath
            let arg = 
                Seq.map Parameters.stringOfClustalParams parameters
                |> String.concat ""
                |> fun x -> (stringOfInputType inputPath) + out + x
            let name = defaultArg name arg
            runProcess rootPath arg name

        ///Runs clustalo tool with given sequences and parameters and returns an alignment
        member this.AlignSequences((input:seq<TaggedSequence<string,char>>),(parameters:seq<Parameters.ClustalParams>),(?name:string)) = 
            let format = 
                let inputFormat = [InputCustom.Format FileFormat.FastA]
                let outPutFormat = [OutputCustom.Format FileFormat.Clustal]
                [ClustalParams.Input inputFormat; ClustalParams.Output outPutFormat]
            let temp = System.IO.Path.GetTempPath()
           
            let inPath = temp + Guid.NewGuid().ToString() + ".fasta"
            let outPath = temp + Guid.NewGuid().ToString() + ".aln"
            
            let inArg = sprintf "-i %s " inPath
            let outArg = sprintf "-o %s " outPath       

            try 
                FastA.write id inPath (input |> Seq.map tsToFasta)
                let arg =
                    let p = 
                        parameters
                        |> Seq.map (fun cParam -> 
                                match cParam with
                                | Output oParam  -> ClustalParams.Output (Seq.filter (fun x -> match x with | OutputCustom.Format _ -> false | _ -> true) oParam)
                                | Input iParam -> ClustalParams.Input (Seq.filter (fun x -> match x with | InputCustom.Format _ -> false | _ -> true) iParam)
                                | _ -> cParam)
                    Seq.map Parameters.stringOfClustalParams (Seq.append p format)
                    |> String.concat ""
                    |> fun x -> inArg + outArg + x
                let name = defaultArg name arg
                runProcess rootPath arg name
                Clustal.ofFile outPath
            finally       
                System.IO.File.Delete inPath
                System.IO.File.Delete outPath


