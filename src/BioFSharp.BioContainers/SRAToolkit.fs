namespace BioFSharp.BioContainers
open BioContainer

module SRATools =

    //type PrefetchParams =
    //    |Placeholder

    //    static member makeCmd = function
    //        |Placeholder -> [""]

    //    static member makeCmdWith (m:MountInfo) = function
    //        |Placeholder -> [""]
    



    //let runPrefetchAsync (bcContext:BioContainer.BcContext) (opt:PrefetchParams list) = 

    //    let cmds = (opt |> List.map (PrefetchParams.makeCmdWith bcContext.Mount))
    //    let tp = "prefetch"::(cmds |> List.concat)

    //    printfn "Starting process prefetch\r\nparameters:"
    //    cmds |> List.iter (fun op -> printfn "\t%s" (String.concat " " op))

    //    async {
    //            let! res = BioContainer.execAsync bcContext tp           
    //            return res
    //    }

    type SplitOptions =
        ///Split spots into reads
        |SplitSpot          
        ///Write reads into different files
        |SplitFiles
        ///Writes single reads into special file
        |Split3   
        ///Writes whole spots into one file
        |ConcatenateReads
        
        static member make = function 
            |SplitSpot          -> "--split-spot"
            |SplitFiles         -> "--split-files"
            |Split3             -> "--split-3"
            |ConcatenateReads   -> "--concatenate-read"

    //type FastQDumpParams =
    //    |Placeholder

    //    static member makeCmd = function
    //        |Placeholder -> [""]

    //    static member makeCmdWith (m:MountInfo) = function
    //        |Placeholder -> [""]
    



    //let runFastQDumpAsync (bcContext:BioContainer.BcContext) (opt:FastQDumpParams list) = 

    //    let cmds = (opt |> List.map (FastQDumpParams.makeCmdWith bcContext.Mount))
    //    let tp = "fastq-dump"::(cmds |> List.concat)

    //    printfn "Starting process fastq-dump\r\nparameters:"
    //    cmds |> List.iter (fun op -> printfn "\t%s" (String.concat " " op))

    //    async {
    //            let! res = BioContainer.execAsync bcContext tp           
    //            return res
    //    }


    ///DSL for command line arguments for the fasterq-dump tool contained in the SRA Toolkit
    type FasterQDumpParams =
        ///full path of outputfile (overrides usage of current directory and given accession)
        |OutFile of string
        ///path for outputfile (overrides usage of current directory, but uses given accession)
        |OutDirectory of string
        ///path to directory for temp. files (dflt=current dir.)
        |TempDirectory of string
        //size of file-buffer (dflt=1MB)
        |BufferSize of string
        ///Determine how to handle paired reads
        |Split of SplitOptions
        ///size of cursor-cache (dflt=10MB, takes number or number and unit)
        |CursorCacheSize of string
        ///memory limit for sorting (dflt=100MB, takes number or number and unit)
        |SortingMemoryLimit of string
        ///how many threads to use (dflt=6)
        |Threads of int
        ///show progress (not possible if stdout used)
        |ShowProgress
        ///print details of all options selected
        |PrintDetails
        ///print output to stdout
        |StdOut
        //force overwrite of existing file(s)
        |Force
        ///use rowid as name (avoids using the name column)
        |RowIdAsName
        ///skip technical reads
        |SkipTechnical
        ///explicitly include technical reads
        |IncludeTechnical
        ///include read-number in defline
        |PrintReadNumber
        ///filter by sequence-lenght
        |MinReadLength of int
        ///which seq-table to use in case of pacbio
        |PacBioTableName of string
        ///terminate on invalid read
        |Strict
        ///filter output by matching against given bases
        |FilterBases of string
        ///append to output-file, instead of overwriting it
        |AppendOutput
        ///path to ngc file
        |NGCFilePath of string
        ///path to permission file
        |PermissionFilePath of string
        ///location in cloud
        |CloudLocation of string
        //path to cart file
        |CartPath of string
        ///disable multithreading
        |DisableMultiThreading
        //Display the version of the program
        |Version
        //Logging level as number or enum string. One of (fatal|sys|int|err|warn|info|debug) or (0-6) Current/default is warn
        |LogLevel of string
        ///Read more options and parameters from the file.
        |OptionFilePath of string
        
        static member makeCmd = function
            |OutFile o              -> ["-o"; o]
            |OutDirectory o         -> ["-O"; o]
            |TempDirectory t        -> ["-t"; t]
            |NGCFilePath n          -> ["--ngc"; n]
            |PermissionFilePath p   -> ["--perm"; p]
            |CartPath c             -> ["--cart"; c]
            |OptionFilePath o       -> ["option-file"; o]
            |Split so               -> [SplitOptions.make so]
            |Threads t              -> ["-e"; string t]
            |MinReadLength l        -> ["-M"; string l]
            |FilterBases b          -> ["-B"; b]
            |BufferSize b           -> ["-b"; b]
            |CursorCacheSize c      -> ["-c"; c]
            |SortingMemoryLimit m   -> ["-m"; m]
            |ShowProgress           -> ["-p"]
            |PrintDetails           -> ["-x"]
            |StdOut                 -> ["-Z"]
            |Force                  -> ["-f"]
            |RowIdAsName            -> ["-N"]
            |PrintReadNumber        -> ["-P"]
            |AppendOutput           -> ["-A"]
            |Version                -> ["-V"]
            |LogLevel l             -> ["-L"; l]
            |SkipTechnical          -> ["--skip-technical"]
            |IncludeTechnical       -> ["--include-technical"]
            |PacBioTableName t      -> ["--table"; t]
            |Strict                 -> ["--strict"]
            |CloudLocation c        -> ["--location"; c]
            |DisableMultiThreading  -> ["--disable-multithreading"]

        static member makeCmdWith (m:MountInfo) = function
            |OutFile o              -> ["-o"; MountInfo.containerPathOf m o]
            |OutDirectory o         -> ["-O"; MountInfo.containerPathOf m o]
            |TempDirectory t        -> ["-t"; MountInfo.containerPathOf m t]
            |NGCFilePath n          -> ["--ngc"      ;  MountInfo.containerPathOf m n]
            |PermissionFilePath p   -> ["--perm"     ;  MountInfo.containerPathOf m p]
            |CartPath c             -> ["--cart"     ;  MountInfo.containerPathOf m c]
            |OptionFilePath o       -> ["option-file";  MountInfo.containerPathOf m o]
            |Split so               -> [SplitOptions.make so]
            |Threads t              -> ["-e"; string t]
            |MinReadLength l        -> ["-M"; string l]
            |FilterBases b          -> ["-B"; b]
            |BufferSize b           -> ["-b"; b]
            |CursorCacheSize c      -> ["-c"; c]
            |SortingMemoryLimit m   -> ["-m"; m]
            |ShowProgress           -> ["-p"]
            |PrintDetails           -> ["-x"]
            |StdOut                 -> ["-Z"]
            |Force                  -> ["-f"]
            |RowIdAsName            -> ["-N"]
            |PrintReadNumber        -> ["-P"]
            |AppendOutput           -> ["-A"]
            |Version                -> ["-V"]
            |LogLevel l             -> ["-L"; l]
            |SkipTechnical          -> ["--skip-technical"]
            |IncludeTechnical       -> ["--include-technical"]
            |PacBioTableName t      -> ["--table"; t]
            |Strict                 -> ["--strict"]
            |CloudLocation c        -> ["--location"; c]
            |DisableMultiThreading  -> ["--disable-multithreading"]


    let runFasterQDumpAsync (bcContext:BioContainer.BcContext) (opt:FasterQDumpParams list) (accession:string) = 

        let cmds = (opt |> List.map (FasterQDumpParams.makeCmdWith bcContext.Mount))
        let tp = "fasterq-dump"::(cmds |> List.concat)@[accession]

        printfn "Starting process fasterq-dump\r\nparameters:"
        cmds |> List.iter (fun op -> printfn "\t%s" (String.concat " " op))

        async {
                let! res = BioContainer.execAsync bcContext tp           
                return res
        }

    let runFasterQDump (bcContext:BioContainer.BcContext) (opt:FasterQDumpParams list) (accession:string) = 
        runFasterQDumpAsync bcContext opt accession
        |> Async.RunSynchronously