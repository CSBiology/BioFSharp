namespace BioFSharp.BioContainers

module Hera =
    open FSharpAux
    open BioContainer

    let ImageHera = Docker.DockerId.ImageId "hera"

    type HeraIndexOutputCustom =
        /// Genome reference file in FASTA format.
        | Fasta of string
        /// Gene annotation file in GTF format.
        | GTF  of string
        /// Prefix name for output index files generated.
        | Prefix of string
        /// Output directory for index files generated. Default is "./".
        | Outdir of string 

        static member makeCmdWith (m:MountInfo) = function   
            | Fasta path  -> [ "-g"; (MountInfo.containerPathOf m path)]
            | GTF path    -> [ "-t"; (MountInfo.containerPathOf m path)]
            | Prefix path -> [ "-p"; path]
            | Outdir path -> [ "-o"; (MountInfo.containerPathOf m path)]

        static member make = function  
            | Fasta path  -> sprintf "-g %s" path
            | GTF path    -> sprintf "-t %s" path
            | Prefix path -> sprintf "-p %s" path
            | Outdir path -> sprintf "-o %s" path


    type HeraQuantOutputCustom =
        /// Prefix name of index files, including directory.
        | IndexDirectory of string
        /// File(s) containing first reads in pair.
        | FirstSource of string
        /// File(s) containing second reads in pair.
        | SecondSource of string
        /// Prefix name for result files. Default is "_out"
        | OutputPrefix of string
        /// Output directory for result files. Default is "./".
        | OutputDirectory of string
        /// Number of threads running in parallel. Default is 1.
        | ThreadNumber of int
        /// Output read alignments in BA; file format.
        | BamFileOutput
        /// Compress level of bam file. Default is 1. 
        | BamCompressionLevel of int


        static member makeCmdWith (m:MountInfo) = function
            | IndexDirectory d          -> [ "-x"; d]
            | FirstSource path          -> [ "-1"; path]
            | SecondSource value        -> [ "-2" ; (MountInfo.containerPathOf m value)]
            | OutputPrefix path         -> [ "-p"; path]
            | OutputDirectory path      -> [ "-o"; (MountInfo.containerPathOf m path)]
            | ThreadNumber value        -> [ "-t"; string value]
            | BamFileOutput             -> [ "-w"]
            | BamCompressionLevel value -> [ "-z"; string value]

        static member make = function
            | IndexDirectory d          -> sprintf "-x %s" d    
            | FirstSource path          -> sprintf "-1 %s" path 
            | SecondSource value        -> sprintf "-2 %s" value
            | OutputPrefix path         -> sprintf "-p %s" path 
            | OutputDirectory path      -> sprintf "-o %s" path 
            | ThreadNumber value        -> sprintf "-t %s" (string value)
            | BamFileOutput             -> sprintf "-w" 
            | BamCompressionLevel value -> sprintf "-z %s" (string value)  


    let runHeraIndexAsync (bcContext:BioContainer.BcContext) (opt:HeraIndexOutputCustom list)  =
        let cmds = (opt |> List.map (HeraIndexOutputCustom.makeCmdWith bcContext.Mount))
        let tp = "Nora"::"index"::(cmds |> List.concat)
        async {
            let! heraResult = BioContainer.execReturnAsync bcContext tp
            return heraResult 
            }
        

    let runHeraIndex (bcContext:BioContainer.BcContext) (opt:HeraIndexOutputCustom list) = 
        runHeraIndexAsync bcContext opt 
        |> Async.RunSynchronously



    let runHeraQuantAsync (bcContext:BioContainer.BcContext) (opt:HeraQuantOutputCustom list)  =
        let cmds = (opt |> List.map (HeraQuantOutputCustom.makeCmdWith bcContext.Mount))
        let tp = "Nora"::"quant"::(cmds |> List.concat)
        async {
            let! noraResult = BioContainer.execAsync bcContext tp
            return noraResult 
            }


    let runHeraQuant (bcContext:BioContainer.BcContext) (opt:HeraQuantOutputCustom list) = 
        runHeraQuantAsync bcContext opt 
        |> Async.RunSynchronously


