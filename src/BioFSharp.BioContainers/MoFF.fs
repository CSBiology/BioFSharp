namespace BioFSharp.BioContainers

///moFF is an OS independent tool designed to extract apex MS1 intensity using a set of identified MS2 peptides. It currently uses a Thermo library to directly extract data from Thermo Raw spectrum files,
///eliminating the need for conversions from other formats. Moreover, moFF also allows to work directly with mzML files.
///Input Data: moFF requires two types of input for the quantification procedure
///Thermo RAW file or mzML file -and- MS2 identified peptide information.
///The MS2 identified peptides can be presented as a tab-delimited file containing mimimal (mandatory) annotation for each peptide.
///The tab-delimited file must contain the following information for all the peptides:
///'peptide' : peptide-spectrum-match sequence
///'prot' : protein ID
///'mod_peptide' : peptide-spectrum-match sequence that contains also possible modification (i.e NH2-M<Mox>LTKFESK-COOH )
///'rt': peptide-spectrum-match retention time (i.e the retention time contained in the mgf file; The retention time must be specified in second)
///'mz' : mass over charge
///'mass' : mass of the peptide
///'charge' : charge of the ionized peptide.
/// ------ Argentini et al. Nature Methods. 2016 12(13):964–966.

module MoFF =

    open BioContainer
    open FSharpAux


    ///File input methods for the different run options.
    type InputOption =
        ///--loc_in                      the folder where the input files are located
        | Ms2PeptidesInputFileFolder of string
        ///--raw_repo                    the folder containing all the raw files Note: not for MatchBetweenRuns
        | RawFilesFolder of string                                         
        ///--tsv_list                    the input file with for MS2 peptides    Note: not for MatchBetweenRuns
        | Ms2PeptidesInputFile of string list                                      
        ///--raw_list                    specify directly the  raw file          Note: not for MatchBetweenRuns
        | RawFile of string list                                                     


        static member makeWith (m: MountInfo) = function
            | Ms2PeptidesInputFileFolder (path)   -> ["--loc_in" ; (MountInfo.containerPathOf m path)]
            | RawFilesFolder (path)               -> ["--raw_repo" ; (MountInfo.containerPathOf m path)]
            | Ms2PeptidesInputFile (pathList)     -> List.append ["--tsv_list"] (pathList |> List.map (fun path -> MountInfo.containerPathOf m path))
            | RawFile (pathList)                  -> List.append ["--raw_list"] (pathList |> List.map (fun path -> MountInfo.containerPathOf m path))

        static member make = function
            | Ms2PeptidesInputFileFolder (path)   -> ["--loc_in" ; path]
            | RawFilesFolder (path)               -> ["--raw_repo" ; path]
            | Ms2PeptidesInputFile (pathList)     -> List.append ["--tsv_list"] pathList 
            | RawFile (pathList)                  -> List.append ["--raw_list"] pathList 


    ///Those options run the MBR module, which takes a list of features of interest for a given run, and then matches the corresponding features in other runs.
    ///The output will be stored in a subfolder ('mbr_output') inside the specified input folder.
    ///The MBR module will consider all the .txt files present in the specified input folder as replicates (to select specific files or
    ///different extension, please refer to the example below). The files in sample_folder/mbr_output will be identical to the input files,
    ///but they will have an additional field ('matched') that specifies which peptides have match (1) or not (0).
    ///The MBR algorithm also produces a log file in the provided input directory.
    type MatchBetweenRunsOptions =
        ///--sample                      reg exp to filter the input file names (only with --loc_in input option-
        | FilterInputFiles of string
        ///--ext                         file extention of the input file. Default .txt)
        | FileExtensionsInputFile of string
        ///--log_label                   filename for the mbr log file. Default moFF_mbr
        | FileNameMBRLogFile of string
        ///--w_filt                      width value for outlier filtering. Default 3
        | WidthForOutlierFiltering of int
        ///--out_flag                    if set, outliers for rt time allignment are filtered. Default value: True
        | OutlierFilter of bool
        ///--w_comb                      if set, RT model combination is weighted using traing model errors: Default value: False
        | WeightedRTModelCombination of bool

        static member makeWith (m: MountInfo) = function
            | FilterInputFiles (name)                 -> ["--sample" ; string name]
            | FileExtensionsInputFile (fileExtension) -> ["--ext" ; string fileExtension]
            | FileNameMBRLogFile (name)               -> ["--log_label" ; string name]
            | WidthForOutlierFiltering (width)        -> ["--w_filt" ; string width]
            | OutlierFilter (boolean)                 -> ["--out_flag" ; string boolean]
            | WeightedRTModelCombination (boolean)    -> ["--w_comb" ; string boolean]

        static member make = function
            | FilterInputFiles (name)                 -> ["--sample" ; string name]
            | FileExtensionsInputFile (fileExtension) -> ["--ext" ; string fileExtension]
            | FileNameMBRLogFile (name)               -> ["--log_label" ; string name]
            | WidthForOutlierFiltering (width)        -> ["--w_filt" ; string width]
            | OutlierFilter (boolean)                 -> ["--out_flag" ; string boolean]
            | WeightedRTModelCombination (boolean)    -> ["--w_comb" ; string boolean]

    ///Those options run the Apex Intensity module.
    ///Around the RT of the input feature, a time window is constructed which results in a local XIC.
    ///The peak apex is then located in this local XIC. The log_L_R metric measures the skewness of the peak around the RT of the obtained apex point.
    ///The SNR metric provides the ratio of peak height to noise, where the noise value is set as the lowest intensity value in the local XIC.
    ///mzML must be specified using --tsv_list | --raw_list (Ms2PeptidesInputFile & RawFile)
    type ApexIntensityOptions =
            ///--tol                         mass tollerance (ppm)
            | MassTolerance of int
            ///--xic_length                  rt windows for xic (minutes). Default value is 3  min
            | RtWindow of float
            ///--rt_peak_win                 time windows used to get the apex for the ms2 peptide/feature  (minutes). Default value is 1
            | GetPeakInRtWindow of float
            ///--rt_peak_win_match           time windows used to get the apex for machted features (minutes). Default value is 1.2
            | GetPeakInRtWindowForMatches of float
            //TODO: check if this needs actually a number, could be that it needs nothing
            ///--peptide_summary             flag that allows have as output the peptided summary intensity file. Default is disable(0)
            | PeptideSummaryFile
            ///--tag_pepsum                  a tag that is used in the peptide summary file name
            | PeptideSummaryFileName of string
            ///--loc_out                     output folder
            | OutputFileFolder of string
            ///--match_filter                If set, filtering on the matched peak is activated. Default value: False
            | FilterMatchedPeak of bool
            ///--ptm_file                    modification json ptm file. Default file ptm_setting.json
            | PtmFile of string
            ///--quantile_thr_filtering      quantile value used to computed the filtering threshold for the matched peak . Default is 0.75
            | FilteringThresholdQuantileValue of float
            ///--sample_size                 percentage of MS2 identified peptides used to estimated the threshold
            | SampleSizeForThreshold of float

            static member makeWith (m: MountInfo) = function
                | MassTolerance (i)                   -> ["--tol" ; string i]
                | RtWindow (f)                        -> ["--xic_length" ; string f]
                | GetPeakInRtWindow (f)               -> ["--rt_peak_win" ; string f]
                | GetPeakInRtWindowForMatches (f)     -> ["--rt_peak_win_match" ; string f]
                //TODO: check if this needs actually a number, could be that it needs nothing
                | PeptideSummaryFile                  -> ["--peptide_summary"]
                | PeptideSummaryFileName (name)       -> ["--tag_pepsum" ; name]
                | OutputFileFolder (path)             -> ["--loc_out" ; (MountInfo.containerPathOf m path)]
                | FilterMatchedPeak (boolean)         -> ["--match_filter" ; string boolean]
                | PtmFile (path)                      -> ["--ptm_file" ; (MountInfo.containerPathOf m path)]
                | FilteringThresholdQuantileValue (f) -> ["--quantile_thr_filtering" ; string f]
                | SampleSizeForThreshold (f)          -> ["--sample_size" ; string f]

            static member make = function
                | MassTolerance (i)                   -> ["--tol" ; string i]
                | RtWindow (f)                        -> ["--xic_length" ; string f]
                | GetPeakInRtWindow (f)               -> ["--rt_peak_win" ; string f]
                | GetPeakInRtWindowForMatches (f)     -> ["--rt_peak_win_match" ; string f]
                //TODO: check if this needs actually a number, could be that it needs nothing
                | PeptideSummaryFile                  -> ["--peptide_summary"]
                | PeptideSummaryFileName (name)       -> ["--tag_pepsum" ; name]
                | OutputFileFolder (path)             -> ["--loc_out" ; path]
                | FilterMatchedPeak (boolean)         -> ["--match_filter" ; string boolean]
                | PtmFile (path)                      -> ["--ptm_file" ; path]
                | FilteringThresholdQuantileValue (f) -> ["--quantile_thr_filtering" ; string f]
                | SampleSizeForThreshold (f)          -> ["--sample_size" ; string f]

    ///Those options run the Entire Workflow module, which executes both, the Match between runs and Apex Intensity module.
    ///The options are identical for both apex and MBR modules. The output for the latter (MBR) is stored in the folder sample_folder/mbr_output,
    ///while the former (apex) generates files in the specified output_moff folder.Log files for both algorithms are generated in the respective folders.
    type EntireWorkflowOptions =
        ///--config_file                 specify a moFF parameter file
        | MoffFile of string
        ///--sample                      reg exp to filter the input file names (only with --loc_in input option-
        | FilterInputFiles of string
        ///--ext                         file extention of the input file. Default .txt)
        | FileExtensionsInputFile of string
        ///--log_label                   filename for the mbr log file. Default moFF_mbr
        | FileNameMBRLogFile of string
        ///--w_filt                      width value for outlier filtering. Default 3
        | WidthForOutlierFiltering of int
        ///--out_flag                    if set, outliers for rt time allignment are filtered. Default value: True
        | OutlierFilter of bool
        ///--w_comb                      if set, RT model combination is weighted using traing model errors: Default value: False
        | WeightedRTModelCombination of bool
        ///--tol                         mass tollerance (ppm)
        | MassTolerance of int
        ///--xic_length                  rt windows for xic (minutes). Default value is 3  min
        | RtWindow of float
        ///--rt_peak_win                 time windows used to get the apex for the ms2 peptide/feature  (minutes). Default value is 1
        | GetPeakInRtWindow of float
        ///--rt_peak_win_match           time windows used to get the apex for machted features (minutes). Default value is 1.2
        | GetPeakInRtWindowForMatches of float
        //TODO: check if this needs actually a number, could be that it needs nothing
        ///--peptide_summary             flag that allows have as output the peptided summary intensity file. Default is disable(0)
        | PeptideSummaryFile
        ///--tag_pepsum                  a tag that is used in the peptide summary file name
        | PeptideSummaryFileName of string
        ///--loc_out                     output folder
        | OutputFileFolder of string
        ///--match_filter                If set, filtering on the matched peak is activated. Default value: False
        | FilterMatchedPeak of bool
        ///--ptm_file                    modification json ptm file. Default file ptm_setting.json
        | PtmFile of string
        ///--quantile_thr_filtering      quantile value used to computed the filtering threshold for the matched peak . Default is 0.75
        | FilteringThresholdQuantileValue of float
        ///--sample_size                 percentage of MS2 identified peptides used to estimated the threshold
        | SampleSizeForThreshold of float

        static member makeWith (m: MountInfo) = function
            | MoffFile (path)                         -> ["--config_file" ; (MountInfo.containerPathOf m path)]
            | FilterInputFiles (name)                 -> ["--sample" ; string name]
            | FileExtensionsInputFile (fileExtension) -> ["--ext" ; string fileExtension]
            | FileNameMBRLogFile (name)               -> ["--log_label" ; string name]
            | WidthForOutlierFiltering (width)        -> ["--w_filt" ; string width]
            | OutlierFilter (boolean)                 -> ["--out_flag" ; string boolean]
            | WeightedRTModelCombination (boolean)    -> ["--w_comb" ; string boolean]
            | MassTolerance (i)                       -> ["--tol" ; string i]
            | RtWindow (f)                            -> ["--xic_length" ; string f]
            | GetPeakInRtWindow (f)                   -> ["--rt_peak_win" ; string f]
            | GetPeakInRtWindowForMatches (f)         -> ["--rt_peak_win_match" ; string f]
            ///TODO: check if this needs actually a number, could be that it needs nothing
            | PeptideSummaryFile                      -> ["--peptide_summary"]
            | PeptideSummaryFileName (name)           -> ["--tag_pepsum" ; name]
            | OutputFileFolder (path)                 -> ["--loc_out" ; (MountInfo.containerPathOf m path)]
            | FilterMatchedPeak (boolean)             -> ["--match_filter" ; string boolean]
            | PtmFile (path)                          -> ["--ptm_file" ; (MountInfo.containerPathOf m path)]
            | FilteringThresholdQuantileValue (f)     -> ["--quantile_thr_filtering" ; string f]
            | SampleSizeForThreshold (f)              -> ["--sample_size" ; string f]

        static member make = function
            | MoffFile (path)                         -> ["--config_file" ; path]
            | FilterInputFiles (name)                 -> ["--sample" ; string name]
            | FileExtensionsInputFile (fileExtension) -> ["--ext" ; string fileExtension]
            | FileNameMBRLogFile (name)               -> ["--log_label" ; string name]
            | WidthForOutlierFiltering (width)        -> ["--w_filt" ; string width]
            | OutlierFilter (boolean)                 -> ["--out_flag" ; string boolean]
            | WeightedRTModelCombination (boolean)    -> ["--w_comb" ; string boolean]
            | MassTolerance (i)                       -> ["--tol" ; string i]
            | RtWindow (f)                            -> ["--xic_length" ; string f]
            | GetPeakInRtWindow (f)                   -> ["--rt_peak_win" ; string f]
            | GetPeakInRtWindowForMatches (f)         -> ["--rt_peak_win_match" ; string f]
            ///TODO: check if this needs actually a number, could be that it needs nothing
            | PeptideSummaryFile                      -> ["--peptide_summary"]
            | PeptideSummaryFileName (name)           -> ["--tag_pepsum" ; name]
            | OutputFileFolder (path)                 -> ["--loc_out" ; path]
            | FilterMatchedPeak (boolean)             -> ["--match_filter" ; string boolean]
            | PtmFile (path)                          -> ["--ptm_file" ; path]
            | FilteringThresholdQuantileValue (f)     -> ["--quantile_thr_filtering" ; string f]
            | SampleSizeForThreshold (f)              -> ["--sample_size" ; string f]

    ///Header type which contains lists of all types for input options.
    type MoFFParameters =
        |Input of InputOption list
        |MatchBetweenRuns of MatchBetweenRunsOptions list
        |ApexIntensity of ApexIntensityOptions list
        |EntireWorkflow of EntireWorkflowOptions list

        static member makeCmdWith (m: MountInfo) = function
            | Input inputOpts          -> inputOpts |> List.collect (InputOption.makeWith m)
            | MatchBetweenRuns mbrOpts -> mbrOpts   |> List.collect (MatchBetweenRunsOptions.makeWith m)
            | ApexIntensity aiOpts     -> aiOpts    |> List.collect (ApexIntensityOptions.makeWith m)
            | EntireWorkflow ewOpts    -> ewOpts    |> List.collect (EntireWorkflowOptions.makeWith m)

        static member makeCmd = function
             | Input inputOpts          -> inputOpts |> List.collect InputOption.make
             | MatchBetweenRuns mbrOpts -> mbrOpts   |> List.collect MatchBetweenRunsOptions.make
             | ApexIntensity aiOpts     -> aiOpts    |> List.collect ApexIntensityOptions.make
             | EntireWorkflow ewOpts    -> ewOpts    |> List.collect EntireWorkflowOptions.make

    ///The output will be stored in a subfolder ('mbr_output') inside the specified input folder. The MBR module will consider all the .txt files present in the specified input folder as replicates.
    ///The files in sample_folder/mbr_output will be identical to the input files, but they will have an additional field ('matched') that specifies which peptides have match (1) or not (0).
    ///The MBR algorithm also produces a log file in the provided input directory.
    let runMatchBetweenRunsAsync (bcContext:BioContainer.BcContext) (opts: MoFFParameters list) =

        let _ =
            opts 
            |> List.filter (fun x -> match x with |Input _ -> false |MatchBetweenRuns _ -> false | _ -> true)
            |> fun x -> if not (List.isEmpty x) then
                            failwith "Options from different run modes are not compatible"

        let input =
            opts
            |> List.filter (fun x -> match x with |Input _ -> true | _ -> false)
            |> fun x -> if List.isEmpty x then
                            failwith "missing file input"
                        elif (List.length(x) > 1) then
                            failwith "too many input options"
                        else
                            MoFFParameters.makeCmdWith bcContext.Mount x.[0]
        let commands =
                opts
                |> List.filter (fun x -> match x with |Input _  -> false |_ -> true)
                |> List.map (MoFFParameters.makeCmdWith bcContext.Mount)

        let tp = ["python"; "moff_all.py"]@input@(commands |> List.concat)@["--mbr"; "only"]

        printfn "Starting process moFF\r\nparameters:"
        (input@(commands |> List.concat)) |> List.iter (fun op -> printfn "\t%s"  op)
        printfn "%s" (String.concat " " tp)

        async{
            let! res = BioContainer.execReturnAsync bcContext tp
            return res
             }

    let runMatchBetweenRuns (bcContext:BioContainer.BcContext) (opts: MoFFParameters list) =
        runMatchBetweenRunsAsync bcContext opts
        |> Async.RunSynchronously

    ///Warning 1: In case of 'ApexIntensityOptions.Ms2PeptidesInputFileFolder' and 'ApexIntensityOptions.RawFilesFolder' raw file names MUST be the same of the input file otherwise the script gives you an error !
    ///Warning 2: You can not mix the two input methods ( 'Ms2PeptidesInputFileFolder' / 'RawFilesFolder' and 'Ms2PeptidesInputFile' / 'RawFile' ) otherwise the script gives you an error !
    ///Warning 3: mzML raw file MUST be only specified using 'RawFile' / 'Ms2PeptidesInputFile'. The 'RawFilesFolder' option is not available for mzML files.
    let runApexIntensityAsync (bcContext:BioContainer.BcContext) (opts: MoFFParameters list) =
 
        let _ =
            opts 
            |> List.filter (fun x -> match x with |Input _ -> false |ApexIntensity _ -> false | _ -> true)
            |> fun x -> if not (List.isEmpty x) then
                            failwith "Options from different run modes are not compatible"
        
        let input =
            opts
            |> List.filter (fun x -> match x with |Input _ -> true | _ -> false)
            |> fun x -> if List.isEmpty x then
                            failwith "missing file input"
                        else
                            List.map (MoFFParameters.makeCmdWith bcContext.Mount) x
        let commands =
            opts
            |> List.filter (fun x -> match x with |Input _  -> false |_ -> true)
            |> List.map (MoFFParameters.makeCmdWith bcContext.Mount)

        let tp = ["python"; "moff_all.py"]@(input |> List.concat)@(commands |> List.concat)@["--mbr"; "off"]

        printfn "Starting process moFF\r\nparameters:"
        ((input|> List.concat)@(commands |> List.concat)) |> List.iter (fun op -> printfn "\t%s"  op)
        printfn "%s" (String.concat " " tp)

        async{
            let! res = BioContainer.execReturnAsync bcContext tp
            return res
             }

    let runApexIntensity (bcContext:BioContainer.BcContext) (opts: MoFFParameters list) =
        runApexIntensityAsync bcContext opts
        |> Async.RunSynchronously

    ///The output for MBR (MatchBetweenRuns) is stored in the folder sample_folder/mbr_output, while apex (ApexIntensity) generates files in the specified output_moff folder. Log files for both algorithms are generated in the respective folders.
    ///Warning 1: In case of 'ApexIntensityOptions.Ms2PeptidesInputFileFolder' and 'ApexIntensityOptions.RawFilesFolder' raw file names MUST be the same of the input file otherwise the script gives you an error !
    ///Warning 2: You can not mix the two input methods ( 'Ms2PeptidesInputFileFolder' / 'RawFilesFolder' and 'Ms2PeptidesInputFile' / 'RawFile' ) otherwise the script gives you an error !
    ///Warning 3: mzML raw file MUST be only specified using 'RawFile' / 'Ms2PeptidesInputFile'. The 'RawFilesFolder' option is not available for mzML files.
    ///You can set all the parameters values in a file and load them using "MoffFile".
    let runEntireWorkflowAsync (bcContext:BioContainer.BcContext) (opts: MoFFParameters list) =
        let _ =
            opts 
            |> List.filter (fun x -> match x with |Input _ -> false |EntireWorkflow _ -> false | _ -> true)
            |> fun x -> if not (List.isEmpty x) then
                            failwith "Options from different run modes are not compatible"
        
        let input =
            opts
            |> List.filter (fun x -> match x with |Input _ -> true | _ -> false)
            |> fun x -> if List.isEmpty x then
                            failwith "missing file input"
                        else
                            List.map (MoFFParameters.makeCmdWith bcContext.Mount) x
        let commands =
            opts
            |> List.filter (fun x -> match x with |Input _  -> false |_ -> true)
            |> List.map (MoFFParameters.makeCmdWith bcContext.Mount)

        let tp = ["python"; "moff_all.py"]@(input |> List.concat)@(commands |> List.concat)@["--mbr"; "all"]

        printfn "Starting process moFF\r\nparameters:"
        ((input|> List.concat)@(commands |> List.concat)) |> List.iter (fun op -> printfn "\t%s"  op)
        printfn "%s" (String.concat " " tp)

        async{
            let! res = BioContainer.execReturnAsync bcContext tp
            return res
             }

    let runEntireWorkflow (bcContext:BioContainer.BcContext) (opts: MoFFParameters list) =
        runEntireWorkflowAsync bcContext opts
        |> Async.RunSynchronously

