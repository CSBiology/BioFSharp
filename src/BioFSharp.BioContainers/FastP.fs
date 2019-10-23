namespace BioFSharp.BioContainers

module FastP =
    open FSharpAux
    open BioContainer

    let ImageFastp = Docker.DockerId.ImageId "fastp"


    type IOOptions =
        ///read1 input file name
        |Input1 of string
        ///read1 output file name
        |Input2 of string
        ///read2 input file name
        |Output1 of string
        ///read2 output file name
        |Output2 of string
        ///indicate the input is using phred64 scoring (it'll be converted to phred33, so the output will still be phred33)
        |Phred64
        ///compression level for gzip output (1 ~ 9). 1 is fastest, 9 is smallest, default is 4.
        |OutputCompressionLevel of int
        ///input from STDIN. If the STDIN is interleaved paired-end FASTQ, please also add --interleaved_in.
        |STDIN
        ///output passing-filters reads to STDOUT. This option will result in interleaved FASTQ output for paired-end input. Disabled by default.
        |STDOUT
        ///indicate that Input1 is an interleaved FASTQ which contains both read1 and read2. Disabled by default.
        |InterleavedInput
        ///specify how many reads/pairs to be processed. Default 0 means process all reads.
        |ReadsToProcess of int
        ///don't overwrite existing files. Overwriting is allowed by default.
        |DontOverwrite

        static member make = function
            |Input1 i                   ->  sprintf "--in1 %s" i
            |Input2 i                   ->  sprintf "--in2 %s" i
            |Output1 i                  ->  sprintf "--out1 %s" i
            |Output2 i                  ->  sprintf "--out2 %s" i
            |Phred64                    ->  "--phred64"
            |OutputCompressionLevel i   ->  sprintf "--compression %i" i
            |STDIN                      ->  "--stdin"
            |STDOUT                     ->  "--stdout"
            |InterleavedInput           ->  "--interleaved_in"
            |ReadsToProcess i           ->  sprintf "--reads_to_process %i" i
            |DontOverwrite              ->  "--dont_overwrite"

        static member makeCmdWith (m:MountInfo) = function
            |Input1 i                   ->  [ "--in1"; (MountInfo.containerPathOf m i) ] 
            |Input2 i                   ->  [ "--in2"; (MountInfo.containerPathOf m i) ] 
            |Output1 i                  ->  [ "--out1"; (MountInfo.containerPathOf m i)] 
            |Output2 i                  ->  [ "--out2"; (MountInfo.containerPathOf m i)] 
            |Phred64                    ->  ["--phred64"]
            |OutputCompressionLevel i   ->  [ "--compression"; string i ]
            |STDIN                      ->  ["--stdin"]
            |STDOUT                     ->  ["--stdout"]
            |InterleavedInput           ->  ["--interleaved_in"]
            |ReadsToProcess i           ->  [ "--reads_to_process"; string i]
            |DontOverwrite              ->  ["--dont_overwrite"]



    type AdapterTrimmingOptions =
        ///adapter trimming is enabled by default. If this option is specified, adapter trimming is disabled.
        |DisableAdapterTrimming
        ///the adapter for read1. For SE data, if not specified, the adapter will be auto-detected. For PE data, this is used if R1/R2 are found not overlapped. 
        |AdapterSequence1 of string
        ///the adapter for read2 (PE data only). This is used if R1/R2 are found not overlapped. If not specified, it will be the same as AdapterSequence1.
        |AdapterSequence2 of string

        static member  make = function
            |DisableAdapterTrimming ->  "--disable_adapter_trimming"
            |AdapterSequence1 a1    ->  sprintf "--adapter_sequence %s" a1
            |AdapterSequence2 a2    ->  sprintf "--adapter_sequence_r2 %s" a2

        static member  makeCmdWith (m:MountInfo) = function
            |DisableAdapterTrimming ->  ["--disable_adapter_trimming"]
            |AdapterSequence1 a1    ->  ["--adapter_sequence"; a1]
            |AdapterSequence2 a2    ->  ["--adapter_sequence_r2"; a2]



    type GlobalTrimmingOptions =
        ///trimming how many bases in front for read1, default is 0.
        |TrimFront1 of int 
        ///trimming how many bases in tail for read1, default is 0.
        |TrimFront2 of int
        ///trimming how many bases in front for read2. If it's not specified, it will follow read1's settings.
        |TrimTail1 of int
        ///trimming how many bases in tail for read2. If it's not specified, it will follow read1's settings.
        |TrimTail2 of int

        static member make = function
            |TrimFront1 tf1 ->  sprintf "--trim_front1 %i" tf1
            |TrimFront2 tf2 ->  sprintf "--trim_front2 %i" tf2
            |TrimTail1 tt1  ->  sprintf "--trim_tail1 %i" tt1
            |TrimTail2 tt2  ->  sprintf "--trim_tail2 %i" tt2

        static member makeCmdWith (m:MountInfo) = function
            |TrimFront1 tf1 ->  ["--trim_front1"; string tf1]
            |TrimFront2 tf2 ->  ["--trim_front2"; string tf2]
            |TrimTail1 tt1  ->  ["--trim_tail1"; string tt1]
            |TrimTail2 tt2  ->  ["--trim_tail2"; string tt2]



    type PolyXTrimmingOptions =
        ///force polyG tail trimming, by default trimming is automatically enabled for Illumina NextSeq/NovaSeq data.
        |PolyGTrimming
        ///the minimum length to detect polyG in the read tail. 10 by default.
        |PolyGMinLength of int
        ///disable polyG tail trimming, by default trimming is automatically enabled for Illumina NextSeq/NovaSeq data.
        |DisablePolyGTrimming
        ///enable polyX trimming in 3' ends.
        |PolyXTrimming
        ///the minimum length to detect polyX in the read tail. 10 by default.
        |PolyXMinLength of int

        static member make = function
            |PolyGTrimming          -> "--trim_poly_g "
            |PolyGMinLength pgl     -> sprintf "--poly_g_min_len %i" pgl
            |DisablePolyGTrimming   -> "--disable_trim_poly_g"
            |PolyXTrimming          -> "--trim_poly_x"
            |PolyXMinLength pxl     -> sprintf "--poly_x_min_len %i" pxl

        static member makeCmdWith (m:MountInfo) = function
            |PolyGTrimming          -> ["--trim_poly_g"]
            |PolyGMinLength pgl     -> [ "--poly_g_min_len"; string pgl]
            |DisablePolyGTrimming   -> ["--disable_trim_poly_g"]
            |PolyXTrimming          -> ["--trim_poly_x"]
            |PolyXMinLength pxl     -> [ "--poly_x_min_len"; string pxl]



    type QualityCuttingOptions =
        ///enable per read cutting by quality in front (5'), default is disabled (WARNING: this will interfere deduplication for both PE/SE data).
        |FivePrimeQualityCut
        ///enable per read cutting by quality in tail (3'), default is disabled (WARNING: this will interfere deduplication for SE data).
        |ThreePrimeQualityCut
        ///the size of the sliding window for sliding window trimming, default is 4.
        |CutWindowSize of int
        ///the bases in the sliding window with mean quality below cutting_quality will be cut, default is Q20.
        |CutMeanQuality of int

        static member make = function
            |FivePrimeQualityCut    ->  "--cut_by_quality5"
            |ThreePrimeQualityCut   ->  "--cut_by_quality3"
            |CutWindowSize cws      ->  sprintf "--cut_window_size %i" cws
            |CutMeanQuality cmq     ->  sprintf "--cut_mean_quality %i" cmq

        static member makeCmdWith (m:MountInfo) = function
            |FivePrimeQualityCut    ->  ["--cut_by_quality5"]
            |ThreePrimeQualityCut   ->  ["--cut_by_quality3"]
            |CutWindowSize cws      ->  ["--cut_window_size"; string cws]
            |CutMeanQuality cmq     ->  ["--cut_mean_quality"; string cmq]


    type QualityFilteringOptions =
        ///quality filtering is enabled by default. If this option is specified, quality filtering is disabled.
        |DisableQualityFiltering
        ///the quality value that a base is qualified. Default 15 means phred quality >=Q15 is qualified.
        |BaseQualityThreshold of int
        ///how many percents of bases are allowed to be unqualified (0~100). Default 40 means 40%.
        |UnqualifiedThreshold of int
        ///if one read's number of N base is >n_base_limit, then this read/pair is discarded. Default is 5.
        |NBaseLimit of int

        static member make = function   
            |DisableQualityFiltering    -> "--disable_quality_filtering"
            |BaseQualityThreshold bqt   -> sprintf "--qualified_quality_phred %i" bqt
            |UnqualifiedThreshold ut    -> sprintf "--unqualified_percent_limit %i" ut
            |NBaseLimit nbl             -> sprintf "--n_base_limit %i" nbl

        static member makeCmdWith (m:MountInfo) = function   
            |DisableQualityFiltering    -> ["--disable_quality_filtering"]
            |BaseQualityThreshold bqt   -> [ "--qualified_quality_phred"; string bqt]
            |UnqualifiedThreshold ut    -> [ "--unqualified_percent_limit"; string ut]
            |NBaseLimit nbl             -> [ "--n_base_limit"; string nbl]


    type LengthFilteringOptions =
        ///length filtering is enabled by default. If this option is specified, length filtering is disabled.
        |DisableLengthFiltering
        ///reads shorter than length_required will be discarded, default is 15.
        |RequiredLength of int
        ///reads longer than length_limit will be discarded, default 0 means no limitation.
        |LengthLimit of int
    
        static member make = function
            |DisableLengthFiltering -> "--disable_length_filtering"
            |RequiredLength rl      -> sprintf "--length_required %i" rl
            |LengthLimit lm         -> sprintf "--length_limit %i" lm

        static member makeCmdWith (m:MountInfo) = function
            |DisableLengthFiltering -> ["--disable_length_filtering"]
            |RequiredLength rl      -> [ "--length_required"; string rl]
            |LengthLimit lm         -> [ "--length_limit"; string lm]


    type LowComplexityFilteringOptions =
        ///enable low complexity filter. The complexity is defined as the percentage of base that is different from its next base (base[i] != base[i+1]).
        |EnableLowComplexityFiltering
        ///the threshold for low complexity filter (0~100). Default is 30, which means 30% complexity is required.
        |ComplexityThreshold of int

        static member make = function
            |EnableLowComplexityFiltering   -> "--low_complexity_filter"
            |ComplexityThreshold ct         -> sprintf "--complexity_threshold %i" ct

        static member makeCmdWith (m:MountInfo) = function
            |EnableLowComplexityFiltering   -> ["--low_complexity_filter"]
            |ComplexityThreshold ct         -> ["--complexity_threshold"; string ct]


    type IndexFilteringOptions =
        ///specify a file contains a list of barcodes of index1 to be filtered out, one barcode per line.
        |FilterByIndex1 of string
        ///specify a file contains a list of barcodes of index2 to be filtered out, one barcode per line.
        |FilterByIndex2 of string
        ///the allowed difference of index barcode for index filtering, default 0 means completely identical.
        |IndexFilterThreshold of int

        static member make = function
            |FilterByIndex1 fbi1        -> sprintf "--filter_by_index1 %s" fbi1
            |FilterByIndex2 fbi2        -> sprintf "--filter_by_index2 %s" fbi2
            |IndexFilterThreshold ift   -> sprintf "--filter_by_index_threshold %i" ift

        static member makeCmdWith (m:MountInfo) = function
            |FilterByIndex1 fbi1        -> ["--filter_by_index1"; fbi1]
            |FilterByIndex2 fbi2        -> ["--filter_by_index2"; fbi2]
            |IndexFilterThreshold ift   -> ["--filter_by_index_threshold %i"; string ift]


    type BaseCorrectionOptions =
        ///enable base correction in overlapped regions (only for PE data), default is disabled.
        |EnableBaseCorrection
        ///the minimum length of the overlapped region for overlap analysis based adapter trimming and correction. 30 by default.
        |RequiredLengthOverlap of int
        ///the maximum difference of the overlapped region for overlap analysis based adapter trimming and correction. 5 by default.
        |MaxOverlapDifference of int

        static member make = function
            |EnableBaseCorrection       -> "--correction" 
            |RequiredLengthOverlap rlo  -> sprintf "--overlap_len_require %i" rlo
            |MaxOverlapDifference modiff   -> sprintf "--overlap_diff_limit %i" modiff

        static member makeCmdWith (m:MountInfo) = function
            |EnableBaseCorrection       -> ["--correction"]
            |RequiredLengthOverlap rlo  -> [ "--overlap_len_require"; string rlo]
            |MaxOverlapDifference modiff ->[  "--overlap_diff_limit"; string modiff]


    type UMILocation =
        |Index1
        |Index2
        |Read1
        |Read2
        |PerIndex
        |PerRead
        |NoLocation

        static member make = function
            |Index1     -> "index1"
            |Index2     -> "index2"
            |Read1      -> "read1"
            |Read2      -> "read2"
            |PerIndex   -> "per_index"
            |PerRead    -> "per_read"
            |NoLocation -> ""


        static member makeCmdWith (m:MountInfo) = function
            |Index1     -> ["index1"]
            |Index2     -> ["index2"]
            |Read1      -> ["read1"]
            |Read2      -> ["read2"]
            |PerIndex   -> ["per_index"]
            |PerRead    -> ["per_read"]
            |NoLocation -> [""]

    //========================================= Helper ==========================================
    let stringOfUMILocation (loc:UMILocation) =
        match loc with
        |Index1     -> "index1"
        |Index2     -> "index2"
        |Read1      -> "read1"
        |Read2      -> "read2"
        |PerIndex   -> "per_index"
        |PerRead    -> "per_read"
        |NoLocation -> ""
    //=============================================================================================

    type UMIProcessingOptions =
        ///enable unique molecular identifer (UMI) preprocessing.
        |EnableUMI
        ///specify the location of UMI, can be index1/index2/read1/read2/per_index/per_read, default is none.
        |Location of UMILocation
        ///if the UMI is in read1/read2, its length should be provided.
        |UMILength of int
        ///if specified, an underline will be used to connect prefix and UMI (i.e. prefix=UMI, UMI=AATTCG, final=UMI_AATTCG). No prefix by default.
        |UMIPrefix of string
        ///if the UMI is in read1/read2, fastp can skip several bases following UMI, default is 0.
        |UMISkip of int

        static member make = function
            |EnableUMI          -> sprintf "--umi"
            |Location loc       -> sprintf "--umi_loc %s" (stringOfUMILocation loc) //(stringOfUMILocation loc)
            |UMILength len      -> sprintf "--umi_len %i" len
            |UMIPrefix prefix   -> sprintf "--umi_prefix %s" prefix
            |UMISkip s          -> sprintf "--umi_skip %i" s

        static member makeCmdWith (m:MountInfo) = function
            |EnableUMI          -> ["--umi"]
            |Location loc       -> ["--umi_loc"; (stringOfUMILocation loc)]   //(stringOfUMILocation loc)
            |UMILength len      -> ["--umi_len"; string len]
            |UMIPrefix prefix   -> ["--umi_prefix"; prefix]
            |UMISkip s          -> ["--umi_skip"; string s]


    type OverrepresentationAnalysisOptions =
        ///enable overrepresented sequence analysis.
        |EnableORA
        ///One in ORASampling reads will be computed for overrepresentation analysis (1~10000), smaller is slower, default is 20.
        |ORASampling of int

        static member make = function
            |EnableORA          -> "--overrepresentation_analysis"
            |ORASampling smpl   -> sprintf "--overrepresentation_sampling %i" smpl

        static member makeCmdWith (m:MountInfo) = function
            |EnableORA          -> ["--overrepresentation_analysis"]
            |ORASampling smpl   -> [ "--overrepresentation_sampling"; string smpl]


    type ReportingOptions =
        ///the json format report file name. default is "fastp.json".
        |JsonReport of string
        ///the html format report file name. default is "fastp.html".
        |HtmlReport of string
        ///should be quoted with ' or ", default is "fastp report".
        |ReportTitle of string    

        static member make = function
            |JsonReport jsonrep ->  sprintf "--json %s" jsonrep 
            |HtmlReport htmlrep ->  sprintf "--html %s" htmlrep 
            |ReportTitle title  ->  sprintf "--report_title %s" title 

        static member makeCmdWith (m:MountInfo) = function
            |JsonReport jsonrep ->  ["--json"; (MountInfo.containerPathOf m jsonrep)]
            |HtmlReport htmlrep ->  ["--html"; (MountInfo.containerPathOf m htmlrep)]
            |ReportTitle title  ->  ["--report_title"; (MountInfo.containerPathOf m title)]


    type OutputSplittingOptions =
        ///split output by limiting total split file number with this option (2~999), a sequential number prefix will be added to output name ( 0001.out.fq, 0002.out.fq...), disabled by default.
        |Split of int
        ///split output by limiting lines of each file with this option(>=1000), a sequential number prefix will be added to output name ( 0001.out.fq, 0002.out.fq...), disabled by default.
        |SplitByLines of int64
        ///the digits for the sequential number padding (1~10), default is 4, so the filename will be padded as 0001.xxx, 0 to disable padding.
        |SplitPrefixDigits of int

        static member make = function
            |Split s                ->  sprintf "--split %i" s
            |SplitByLines sbl       ->  sprintf "--split_by_lines %i" sbl
            |SplitPrefixDigits spd  ->  sprintf "--split_prefix_digits %i" spd

        static member makeCmdWith (m:MountInfo) = function
            |Split s                -> ["--split"; string s]
            |SplitByLines sbl       -> ["--split_by_lines"; string sbl]
            |SplitPrefixDigits spd  -> ["--split_prefix_digits"; string spd]



    type FastpParams =
        ///worker thread number, default is 2
        |NumThreads                 of int
        |IO                         of IOOptions list
        |AdapterTrimming            of AdapterTrimmingOptions list
        |GlobalTrimming             of GlobalTrimmingOptions list
        |PolyXTrimming              of PolyXTrimmingOptions list
        |QualityCutting             of QualityCuttingOptions list
        |QualityFiltering           of QualityFilteringOptions list
        |LengthFiltering            of LengthFilteringOptions list
        |LowComplexityFiltering     of LowComplexityFilteringOptions list
        |IndexFiltering             of IndexFilteringOptions list
        |BaseCorrection             of BaseCorrectionOptions list
        |UMIProcessing              of UMIProcessingOptions list
        |OverrepresentationAnalysis of OverrepresentationAnalysisOptions list
        |Reporting                  of ReportingOptions list
        |OutputSplitting            of OutputSplittingOptions list

        static member make = function 
            |NumThreads t  -> [sprintf "--thread %i" t ]
            |IO l ->  
                let tmp = l |> Seq.map (fun p -> IOOptions.make p) |> Seq.toList
                tmp
            |AdapterTrimming l -> 
                let tmp = l |> Seq.map (fun p -> AdapterTrimmingOptions.make p) |> Seq.toList
                tmp
            | GlobalTrimming l ->
                let tmp = l |> Seq.map (fun p -> GlobalTrimmingOptions.make p) |> Seq.toList
                tmp
            |PolyXTrimming l   -> 
                let tmp = l |> Seq.map (fun p -> PolyXTrimmingOptions.make p) |> Seq.toList
                tmp
            |QualityCutting l -> 
                let tmp = l |> Seq.map (fun p -> QualityCuttingOptions.make p) |> Seq.toList
                tmp
            |QualityFiltering l -> 
                let tmp = l |> Seq.map (fun p -> QualityFilteringOptions.make p) |> Seq.toList
                tmp
            |LengthFiltering l -> 
                let tmp = l |> Seq.map (fun p -> LengthFilteringOptions.make p) |> Seq.toList
                tmp
            |LowComplexityFiltering l -> 
                let tmp = l |> Seq.map (fun p -> LowComplexityFilteringOptions.make p) |> Seq.toList
                tmp
            |IndexFiltering l -> 
                let tmp = l |> Seq.map (fun p -> IndexFilteringOptions.make p) |> Seq.toList
                tmp
            |BaseCorrection l -> 
                let tmp = l |> Seq.map (fun p -> BaseCorrectionOptions.make p) |> Seq.toList
                tmp
            |UMIProcessing l -> 
                let tmp = l |> Seq.map (fun p -> UMIProcessingOptions.make p) |> Seq.toList
                tmp
            |OverrepresentationAnalysis l ->
                let tmp = l |> Seq.map (fun p -> OverrepresentationAnalysisOptions.make p) |> Seq.toList
                tmp
            |Reporting l -> 
                let tmp = l |> Seq.map (fun p -> ReportingOptions.make p) |> Seq.toList
                tmp
            |OutputSplitting l -> 
                let tmp = l |> Seq.map (fun p -> OutputSplittingOptions.make p) |> Seq.toList
                tmp


        static member makeCmd (m:MountInfo) = function 
            |NumThreads t  -> [sprintf "--thread %i" t ]
            |IO l ->  
                let tmp = l |> Seq.map (fun p -> IOOptions.makeCmdWith m p) |> List.concat
                tmp
            |AdapterTrimming l -> 
                let tmp = l |> Seq.map (fun p -> AdapterTrimmingOptions.makeCmdWith m p) |> List.concat
                tmp
            | GlobalTrimming l ->
                let tmp = l |> Seq.map (fun p -> GlobalTrimmingOptions.makeCmdWith m p) |> List.concat
                tmp
            |PolyXTrimming l   -> 
                let tmp = l |> Seq.map (fun p -> PolyXTrimmingOptions.makeCmdWith m p) |> List.concat
                tmp
            |QualityCutting l -> 
                let tmp = l |> Seq.map (fun p -> QualityCuttingOptions.makeCmdWith m p) |> List.concat
                tmp
            |QualityFiltering l -> 
                let tmp = l |> Seq.map (fun p -> QualityFilteringOptions.makeCmdWith m p) |> List.concat
                tmp
            |LengthFiltering l -> 
                let tmp = l |> Seq.map (fun p -> LengthFilteringOptions.makeCmdWith m p) |> List.concat
                tmp
            |LowComplexityFiltering l -> 
                let tmp = l |> Seq.map (fun p -> LowComplexityFilteringOptions.makeCmdWith m p) |> List.concat
                tmp
            |IndexFiltering l -> 
                let tmp = l |> Seq.map (fun p -> IndexFilteringOptions.makeCmdWith m p) |> List.concat
                tmp
            |BaseCorrection l -> 
                let tmp = l |> Seq.map (fun p -> BaseCorrectionOptions.makeCmdWith m p) |> List.concat
                tmp
            |UMIProcessing l -> 
                let tmp = l |> Seq.map (fun p -> UMIProcessingOptions.makeCmdWith m p) |> List.concat
                tmp
            |OverrepresentationAnalysis l ->
                let tmp = l |> Seq.map (fun p -> OverrepresentationAnalysisOptions.makeCmdWith m p) |> List.concat
                tmp
            |Reporting l -> 
                let tmp = l |> Seq.map (fun p -> ReportingOptions.makeCmdWith m p) |> List.concat
                tmp
            |OutputSplitting l -> 
                let tmp = l |> Seq.map (fun p -> OutputSplittingOptions.makeCmdWith m p) |> List.concat
                tmp


    let runFastpAsync (bcContext:BioContainer.BcContext) (opt:FastpParams list) =
        let cmds = (opt |> List.map (FastpParams.makeCmd bcContext.Mount))
        let tp = "fastp"::(cmds |> List.concat)

        async {
            let! fastP = BioContainer.execAsync bcContext tp
            return fastP 
            }
        

    let runFastp (bcContext:BioContainer.BcContext) (opt:FastpParams list) = 
        runFastpAsync bcContext opt 
        |> Async.RunSynchronously

