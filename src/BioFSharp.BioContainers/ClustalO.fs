namespace BioFSharp.BioContainers

module ClustalO =

    open FSharpAux
    open BioContainer

//    SEQUENCE INPUT
//        -i, --in, --infile={<file>,-}
//	        Multiple sequence input file (- for stdin)
//
//        --hmm-in=<file>
//	        HMM input files
//
//        --dealign
//	        Dealign input sequences
//
//        --profile1, --p1=<file>
//	        Pre-aligned multiple sequence file (aligned columns will be kept fixed)
//
//        --profile2, --p2=<file>
//	        Pre-aligned multiple sequence file (aligned columns will be kept fixed)
//
//        --is-profile
//	        disable check if profile, force profile (default no)
//
//        -t, --seqtype={Protein, RNA, DNA} 
//	        Force a sequence type (default: auto)
//
//        --infmt={a2m=fa[sta],clu[stal],msf,phy[lip],selex,st[ockholm],vie[nna]} 
//	        Forced sequence input file format (default: auto)


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
        static member make = function
            | FastA     -> "fa"
            | Clustal   -> "clu"
            | MSF       -> "msf"
            | Phylip    -> "phy"
            | Selex     -> "selex"
            | Stockholm -> "st"
            | Vienna    -> "vie"

    ///Types of sequences
    type SeqType = 
        | Protein
        | DNA
        | RNA

        static member make = function
            | Protein   -> "--seqtype=Protein"
            | RNA       -> "--seqtype=RNA"
            | DNA       -> "--seqtype=DNA"

    type InputCustom =
        ///Forced sequence input file format (default: auto)
        | Format of FileFormat
        ///Dealign input sequences
        | Dealign
        ///Disable check if profile, force profile (default no)
        | IsProfile
        ///Force a sequence type (default: auto)
        | SeqType of SeqType    
    
        static member make = function
            | Format f  -> sprintf "--infmt=%s" (FileFormat.make f)
            | Dealign   -> "--dealign "
            | IsProfile -> "--is-profile "
            | SeqType s -> SeqType.make s

    ///Specify the type of input and assign file path
    type FileInput = 
        ///Use this option to make a multiple alignment from a set of sequences. A sequence file must contain more than one sequence (at least two sequences).
        | SequenceFile of string 
        ///Use this option to align two alignments (profiles) together.
        | TwoProfiles of string * string 
        /// Use this option to add new sequences to an existing alignment.
        | SequenceFileAndProfile of string * string
        /// Use this option to make a new multiple alignment of sequences from the input file and use the HMM as a guide (EPA).
        | SequenceFileAndHMM of string * string
    
        static member make = function
            | SequenceFile path                     -> ["-i"; path]
            | TwoProfiles (path1,path2)             -> [sprintf "--p1=%s" path1 ; sprintf"--p2=%s" path2]
            | SequenceFileAndProfile (path1,path2)  -> ["-i"; path1; sprintf "--p1=%s " path2]
            | SequenceFileAndHMM (path1,path2)      -> ["-i"; path1; sprintf "--hmm-in=%s" path2]

        static member makeWith (m: MountInfo) = 
            let cPath p = (MountInfo.containerPathOf m p)
            function
            | SequenceFile path                     -> ["-i"; (MountInfo.containerPathOf m path)]
            | TwoProfiles (path1,path2)             -> [sprintf "--p1=%s" (cPath path1) ; sprintf "--p2=%s" (cPath path2)]
            | SequenceFileAndProfile (path1,path2)  -> ["-i"; (cPath path1); sprintf "--p1=%s " (cPath path2)]
            | SequenceFileAndHMM (path1,path2)      -> ["-i"; (cPath path1); sprintf "--hmm-in=%s" (cPath path2)]

//    ALIGNMENT OUTPUT
//  -o, --out, --outfile={file,-} 
//	    Multiple sequence alignment output file (default: stdout)
//
//  --outfmt={a2m=fa[sta],clu[stal],msf,phy[lip],selex,st[ockholm],vie[nna]} 
//	    MSA output file format (default: fasta)
//
//  --residuenumber, --resno  
//	    in Clustal format print residue numbers (default no)
//
//  --wrap=<n>  
//	    number of residues before line-wrap in output
//
//  --output-order={input-order,tree-order} 
//	    MSA output order like in input/guide-tree

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

        static member make = function
            | Format f -> sprintf "--outfmt=%s" (FileFormat.make f)
            | ResidueNumber -> "--residuenumber"
            | Wrap i -> sprintf "--wrap=%i" i
            | OutputOrderAsTree -> "--output-order=tree-order"


        

    ///Collection of parameters for specifying clustalo alignment
//    CLUSTERING
//  --distmat-in=<file>
//	    Pairwise distance matrix input file (skips distance computation)
//
//  --distmat-out=<file>
//	    Pairwise distance matrix output file
//
//  --guidetree-in=<file>
//	    Guide tree input file
//	    (skips distance computation and guide tree clustering step)
//
//  --guidetree-out=<file>
//	    Guide tree output file
//
//  --full
//	    Use full distance matrix for guide-tree calculation (slow; mBed is default)
//
//  --full-iter
//	    Use full distance matrix for guide-tree calculation during iteration (mBed is default)
//
//  --cluster-size=<n>        
//	    soft maximum of sequences in sub-clusters
//
//  --clustering-out=<file>   
//	    Clustering output file
//
//  --use-kimura
//      use Kimura distance correction for aligned sequences (default no)
//
//  --percent-id
//	    convert distances into percent identities (default no)
//

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
    
        static member make = function
            | DistanceMatrixInput path  -> [sprintf "--distmat-in=%s" path      ]
            | DistanceMatrixOutput path -> [sprintf "--distmat-out=%s" path     ]
            | GuideTreeInput path       -> [sprintf "--guidetree-in=%s" path    ]
            | GuideTreeOutput path      -> [sprintf "--guidetree-out=%s" path   ]
            | Full                      -> ["--full"                            ]
            | FullIter                  -> ["--full-iter"                       ]
            | ClusterSize i             -> [sprintf "--cluster-size=%i" i       ]
            | ClusteringOut path        -> [sprintf "--clustering-out=%s" path  ]
            | UseKimura                 -> ["--use-kimura"                      ]
            | PercentID                 -> ["--percent-id"                      ]

        static member makeWith (m: MountInfo) = 
            let cPath p = (MountInfo.containerPathOf m p)
            function
            | DistanceMatrixInput path  -> [sprintf "--distmat-in=%s" (cPath path)      ]
            | DistanceMatrixOutput path -> [sprintf "--distmat-out=%s" (cPath path)     ]
            | GuideTreeInput path       -> [sprintf "--guidetree-in=%s" (cPath path)    ]
            | GuideTreeOutput path      -> [sprintf "--guidetree-out=%s" (cPath path)   ]
            | Full                      -> ["--full"]
            | FullIter                  -> ["--full-iter"]
            | ClusterSize i             -> [sprintf "--cluster-size=%i" i]
            | ClusteringOut path        -> [sprintf "--clustering-out=%s" (cPath path)  ]
            | UseKimura                 -> ["--use-kimura"]
            | PercentID                 -> ["--percent-id"]

//ITERATION:
//
//  --iterations, --iter=<n>  Number of (combined guide tree/HMM) iterations
//
//  --max-guidetree-iterations=<n> Maximum guide tree iterations
//
//  --max-hmm-iterations=<n>  Maximum number of HMM iterations

    ///Specify maximum number of iterations for given step
    type IterationCustom =
        /// Number of (combined guide tree/HMM) iterations
        | Iterations of int
        /// Maximum guide tree iterations
        | MaxGuideTreeIterations of int
        ///  Maximum number of HMM iterations
        | MaxHMMIterations of int
    
        static member make = function
            | Iterations i              -> [sprintf "--iter=%i" i]
            | MaxGuideTreeIterations i  -> [sprintf "--max-guidetree-iterations=%i" i]
            | MaxHMMIterations i        -> [sprintf "--max-hmm-iterations=%i" i]


//LIMITS (will exit early, if exceeded):
//
//  --maxnumseq=<n>           Maximum allowed number of sequences
//
//  --maxseqlen=<l>           Maximum allowed sequence length
    /// Will exit early, if exceeded
    type LimitsCustom =
        /// Maximum allowed number of sequences
        | MaxSeqNumber of int
        /// Maximum allowed sequence length
        | MaxSeqLength of int
    
        static member make = function
            | MaxSeqNumber i -> [sprintf "--maxnumseq=%i" i]
            | MaxSeqLength i -> [sprintf "--maxseqlen=%i" i]


//MISCELLANEOUS:
//
//  --auto                    Set options automatically (might overwrite some of your options)
//
//  --threads=<n>             Number of processors to use
//
//  -l, --log=<file>          Log all non-essential output to this file
//
//  -h, --help                Print help and exit
//
//  -v, --verbose             Verbose output (increases if given multiple times)
//
//  --version                 Print version information and exit
//
//  --long-version            Print long version information and exit
//
//  --force                   Force file overwriting

    ///Optional, miscallaneous modifiers 
    type MiscellaneousCustom =
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

        static member make = function
            | Auto          -> ["--auto"]
            | Threads i     -> [sprintf "--threads=%i" i]
            | Log s         -> [sprintf "--log=%s" s]
            //| Help -> "--help "
            | VerboseLevel i-> 
                if i > 0 && i < 4 then
                    [for n = 0 to i-1 do yield "-v"]
                elif i > 3 then
                    ["-v"; "-v"; "-v"]
                else
                    []
            | Version       -> ["--version"]
            | LongVersion   -> ["--long-version"]
            | Force         -> ["--force"]

        static member makeWith (m: MountInfo) = 
            let cPath p = (MountInfo.containerPathOf m p)
            function
            | Auto          -> ["--auto"]
            | Threads i     -> [sprintf "--threads=%i" i]
            | Log path      -> [sprintf "--log=%s" (cPath path)]
            //| Help -> "--help "
            | VerboseLevel i-> 
                if i > 0 then
                    [for n = 0 to i-1 do yield "-v"]
                else
                    []
            | Version       -> ["--version"]
            | LongVersion   -> ["--long-version"]
            | Force         -> ["--force"]

    type ClustalOParams = 
        /// Specify input parameters
        | Input         of FileInput * InputCustom list
        /// Specify output parameters
        | Output        of string * OutputCustom list
        /// Specify clustering parameters
        | Clustering    of ClusteringCustom list
        /// Specify iteration parameters
        | Iteration     of IterationCustom list
        /// Specify limits parameters
        | Limits        of LimitsCustom list
        /// Specify miscallaneous parameters
        | Miscellaneous of MiscellaneousCustom list

        static member makeCmd = function
            | Input (i,p)       ->  let tmp = 
                                        p 
                                        |> List.map InputCustom.make
                                    (FileInput.make i)@tmp
            | Output (o,p)      ->  let tmp = 
                                        p 
                                        |> List.map OutputCustom.make
                                    ["-o"; o]@tmp
            | Clustering cl     -> cl   |> List.map ClusteringCustom.make   |> List.concat
            | Iteration it      -> it   |> List.map IterationCustom.make    |> List.concat
            | Limits l          -> l    |> List.map LimitsCustom.make       |> List.concat
            | Miscellaneous misc-> misc |> List.map MiscellaneousCustom.make|> List.concat


        static member makeCmdWith (m: MountInfo) = 
            let cPath p = (MountInfo.containerPathOf m p)
            function
            | Input (i,p)       ->  let tmp = 
                                        p 
                                        |> List.map InputCustom.make
                                    (FileInput.makeWith m i)@tmp
            | Output (o,p)      ->  let tmp = 
                                        p 
                                        |> List.map OutputCustom.make
                                    ["-o"; (cPath o)]@tmp
            | Clustering cl     -> cl   |> List.map (ClusteringCustom.makeWith m)   |> List.concat
            | Iteration it      -> it   |> List.map IterationCustom.make            |> List.concat
            | Limits l          -> l    |> List.map LimitsCustom.make               |> List.concat
            | Miscellaneous misc-> misc |> List.map (MiscellaneousCustom.makeWith m)|> List.concat


    let runClustalOAsync (bcContext:BioContainer.BcContext) (opt:ClustalOParams list) = 

        let cmds = (opt |> List.map (ClustalOParams.makeCmdWith bcContext.Mount))
        let tp = "clustalo"::(cmds |> List.concat)

        printfn "Starting process clustalo\r\nparameters:"
        cmds |> List.iter (fun op -> printfn "\t%s" (String.concat " " op))

        async {
                let! res = BioContainer.execAsync bcContext tp           
                return res
        }

    let runClustalO (bcContext:BioContainer.BcContext) (opt:ClustalOParams list) = 
        runClustalOAsync bcContext opt
        |> Async.RunSynchronously


