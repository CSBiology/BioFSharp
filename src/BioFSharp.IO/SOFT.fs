namespace BioFSharp.IO

[<AutoOpen>]
module SOFT =
    
    module Generated =
        
        module Specifications =
    
            type SOFTPlatformSpecifications = 
            ///Provide an identifier for this entity. This identifier is used only as an internal reference within a given file. The identifier will not appear on final GEO records. 
            | Accession of string
            ///"Provide a unique title that describes your Platform. We suggest that you use the system [institution/lab]-[species]-[number of features]-[version], e.g. ""FHCRC Mouse 15K v1.0""." 
            | Title of string
            ///"Microarrays are 'commercial', 'non-commercial', or 'custom-commercial' in accordance with how the array was manufactured. Use 'virtual' only if creating a virtual definition for MPSS, SARST, or RT-PCR data." 
            | Distribution of string
            ///Select the category that best describes the Platform technology. 
            | Technology of string
            ///Identify the organism(s) from which the features on the Platform were designed or derived. 
            | Organism of string
            ///"Provide the name of the company, facility or laboratory where the array was manufactured or produced." 
            | Manufacturer of string
            ///"Describe the array manufacture protocol. Include as much detail as possible, e.g., clone/primer set identification and preparation, strandedness/length, arrayer hardware/software, spotting protocols. You can include as much text as you need to thoroughly describe the protocol; it is strongly recommended that complete protocol descriptions are provided within your submission." 
            | ManufactureProtocol of string
            ///Provide the manufacturer catalog number for commercially-available arrays. 
            | CatalogNumber of string
            ///Specify a Web link that directs users to supplementary information about the array. Please restrict to Web sites that you know are stable. 
            | WebLink of string
            ///"Provide the surface type of the array, e.g., glass, nitrocellulose, nylon, silicon, unknown." 
            | Support of string
            ///"Provide the coating of the array, e.g., aminosilane, quartz, polysine, unknown." 
            | Coating of string
            ///"Provide any additional descriptive information not captured in another field, e.g., array and/or feature physical dimensions, element grid system." 
            | Description of string
            ///List all people associated with this array design. 
            | Contributor of string
            ///Specify a valid PubMed identifier (PMID) that references a published article that describes the array. 
            | PubmedId of string
            ///Only use for performing�updates�to existing GEO records. 
            | GeoAccession of string
            ///Indicates the start of the data table. 
            | TableBegin of string
            ///Indicates the end of the data table. 
            | TableEnd of string
            ///Custom Attributes Used in the SOFT file
            | AdditionalAttribute of string*string
            ///Table formatted data header
            | TableHeader of string*string
            ///Table formatted data
            | TableData of string
    
            type SOFTSeriesSpecifications = 
            ///Provide an identifier for this entity. This identifier is used only as an internal reference within a given file. The identifier will not appear on final GEO records. 
            | Accession of string
            ///Provide a unique title that describes the overall study. 
            | Title of string
            ///Summarize the goals and objectives of this study. The abstract from the associated publication may be suitable. You can include as much text as you need to thoroughly describe the study. 
            | Summary of string
            ///"Provide a description of the experimental design. Indicate how many Samples are analyzed, if replicates are included, are there control and/or reference Samples, dye-swaps, etc." 
            | OverallDesign of string
            ///"Specify a valid PubMed identifier (PMID) that references a published article describing this study. Most commonly, this information is not available at the time of submission - it can be added later once the data are published." 
            | PubmedId of string
            ///Specify a Web link that directs users to supplementary information about the study. Please restrict to Web sites that you know are stable. 
            | WebLink of string
            ///List all people associated with this study. 
            | Contributor of string
            ///"Indicate the variable type(s) investigated in this study, e.g.,"!Series_variable_1 = age!Series_variable_2 = age"NOTE - this information is optional and does not appear in Series records or downloads, but will be used to assemble corresponding GEO DataSet records." 
            | Variable of int * string
            ///"Describe each variable, e.g.,"!Series_variable_description_1 = 2 months!Series_variable_description_2 = 12 months"NOTE - this information is optional and does not appear in Series records or downloads, but will be used to assemble corresponding GEO DataSet records." 
            | VariableDescription of int * string
            ///"List which Samples belong to each group, e.g.,""!Series_variable_sample_list_1 = samA, samB""!Series_variable_sample_list_2 = samC, samD""NOTE - this information is optional and does not appear in Series records or downloads, but will be used to assemble corresponding GEO DataSet records." 
            | VariableSampleList of int * (string list)
            ///"Indicate the repeat type(s), e.g.,"!Series_repeats_1 = biological replicate!Series_repeats_2 = biological replicate"NOTE - this information is optional and does not appear in Series records or downloads, but will be used to assemble corresponding GEO DataSet records." 
            | Repeats of int * string
            ///"List which Samples belong to each group, e.g.,""!Series_repeats_sample_list_1 = samA, samB""!Series_repeats_sample_list_2 = samC, samD""NOTE - this information is optional and does not appear in Series records or downloads, but will be used to assemble corresponding GEO DataSet records." 
            | RepeatsSampleList of int * (string list)
            ///"Reference the Samples that make up this experiment. Reference the Sample accession numbers (GSMxxx) if the Samples already exists in GEO, or reference the ^Sample identifiers if they are being submitted in the same file." 
            | SampleId of string
            ///Only use for performing�updates�to existing GEO records. 
            | GeoAccession of string
            ///"Indicates the type(s) of experiment conducted in the Series" 
            | Type of string
            ///"Time of submission to GEO" 
            | SubmissionDate of string
            ///Custom Attributes Used in the SOFT file
            | AdditionalAttribute of string*string
    
            type SOFTSampleSpecifications = 
            ///Provide an identifier for this entity. This identifier is used only as an internal reference within a given file. The identifier will not appear on final GEO records. 
            | Accession of string
            ///"Provide a unique title that describes this Sample. We suggest that you use the system [biomaterial]-[condition(s)]-[replicate number], e.g., Muscle_exercised_60min_rep2." 
            | Title of string
            ///"Examples of supplementary file types include original Affymetrix CEL and EXP files, GenePix GPR files, and TIFF image files. Supplementary files should be zipped or tarred together with the SOFT file at time of submission (do not include any sub-directories or sub-folders in your zip/tar archive). Provision of supplementary raw data files facilitates the unambiguous interpretation of data and potential verification of conclusions as set forth in the MIAME guidelines." 
            | SupplementaryFile of int * string
            ///- Affymetrix CHP file name:"If your processed data are CHP files, you can reference the CHP file name in this field. If your manuscript discusses data processed by RMA or another algorithm, we recommend providing those values in the�table section. There is no need to specify the !Sample_platform_id when CHP files are supplied. All external files should be zipped or tarred together with the SOFT file at time of submission."- Tab-delimited table file name:"If it is convenient for you to generate, you can reference the name of an external tab-delimited table file (see format) in this field, rather than include the table in the !Sample_table_begin section. All external files should be zipped or tarred together with the SOFT file at time of submission." 
            | Table of string
            ///"Briefly identify the biological material and the experimental variable(s), e.g., vastus lateralis muscle, exercised, 60 min." 
            | SourceName of int * string
            ///Identify the organism(s) from which the biological material was derived. 
            | Organism of int * string
            ///"Describe all available characteristics of the biological source, including factors not necessarily under investigation. Provide in 'Tag: Value' format, where 'Tag' is a type of characteristic (e.g. ""gender"", ""strain"", ""tissue"", ""developmental stage"", ""tumor stage"", etc), and 'Value' is the value for each tag (e.g. ""female"", ""129SV"", ""brain"", ""embryo"", etc). Include as many characteristics fields as necessary to thoroughly describe your Samples." 
            | Characteristics of int * string
            ///"Specify the name of the company, laboratory or person that provided the biological material." 
            | BiomaterialProvider of int * string
            ///Describe any treatments applied to the biological material prior to extract preparation. You can include as much text as you need to thoroughly describe the protocol; it is strongly recommended that complete protocol descriptions are provided within your submission. 
            | TreatmentProtocol of int * string
            ///Describe the conditions that were used to grow or maintain organisms or cells prior to extract preparation. You can include as much text as you need to thoroughly describe the protocol; it is strongly recommended that complete protocol descriptions are provided within your submission. 
            | GrowthProtocol of int * string
            ///Specify the type of molecule that was extracted from the biological material. 
            | Molecule of int * string
            ///Describe the protocol used to isolate the extract material. You can include as much text as you need to thoroughly describe the protocol; it is strongly recommended that complete protocol descriptions are provided within your submission. 
            | ExtractProtocol of int * string
            ///"Specify the compound used to label the extract e.g., biotin, Cy3, Cy5, 33P." 
            | Label of int * string
            ///Describe the protocol used to label the extract. You can include as much text as you need to thoroughly describe the protocol; it is strongly recommended that complete protocol descriptions are provided within your submission. 
            | LabelProtocol of int * string
            ///"Describe the protocols used for hybridization, blocking and washing, and any post-processing steps such as staining. You can include as much text as you need to thoroughly describe the protocol; it is strongly recommended that complete protocol descriptions are provided within your submission." 
            | HybProtocol of string
            ///"Describe the scanning and image acquisition protocols, hardware, and software. You can include as much text as you need to thoroughly describe the protocol; it is strongly recommended that complete protocol descriptions are provided within your submission." 
            | ScanProtocol of string
            ///"Provide details of how data in the VALUE column of your table were generated and calculated, i.e., normalization method, data selection procedures and parameters, transformation algorithm (e.g., MAS5.0), and scaling parameters. You can include as much text as you need to thoroughly describe the processing procedures." 
            | DataProcessing of string
            ///"Include any additional information not provided in the other fields, or paste in broad descriptions that cannot be easily dissected into the other fields." 
            | Description of string
            ///"Reference the Platform upon which this hybridization was performed. Reference the Platform accession number (GPLxxx) if the Platform already exists in GEO, or reference the ^Platform identifier if the Platform record is being batch submitted within the same SOFT file. To identify the accession number of an existing commercial Platform in GEO, use the�FIND PLATFORM�tool." 
            | PlatformId of string
            ///Only use for performing�updates�to existing GEO records. 
            | GeoAccession of string
            ///Use for SAGE submissions only. 
            | Anchor of string
            ///Use for SAGE submissions only. 
            | Type of string
            ///Use for SAGE submissions only. 
            | TagCount of string
            ///Use for SAGE submissions only. 
            | TagLength of string
            ///Indicates the start of the data table. 
            | TableBegin of string
            ///Indicates the end of the data table. 
            | TableEnd of string
            ///"Sample relation, e.g. SRA accession or BioSample id" 
            | Relation of string
            ///Custom Attributes Used in the SOFT file
            | AdditionalAttribute of string*string
            ///Table formatted data header
            | TableHeader of string*string
            ///Table formatted data
            | TableData of string
    
        module internal Lexing =
            
            open Specifications
    
            [<RequireQualifiedAccess>]
            module Platform =
    
                let (|Accession|_|) ((k:string),(v:string)) =
                    if k="^PLATFORM" then
                        Some(SOFTPlatformSpecifications.Accession v)
                    else
                        None
                let (|Title|_|) ((k:string),(v:string)) =
                    if k="!Platform_title" then
                        Some(SOFTPlatformSpecifications.Title v)
                    else
                        None
                let (|Distribution|_|) ((k:string),(v:string)) =
                    if k="!Platform_distribution" then
                        Some(SOFTPlatformSpecifications.Distribution v)
                    else
                        None
                let (|Technology|_|) ((k:string),(v:string)) =
                    if k="!Platform_technology" then
                        Some(SOFTPlatformSpecifications.Technology v)
                    else
                        None
                let (|Organism|_|) ((k:string),(v:string)) =
                    if k="!Platform_organism" then
                        Some(SOFTPlatformSpecifications.Organism v)
                    else
                        None
                let (|Manufacturer|_|) ((k:string),(v:string)) =
                    if k="!Platform_manufacturer" then
                        Some(SOFTPlatformSpecifications.Manufacturer v)
                    else
                        None
                let (|ManufactureProtocol|_|) ((k:string),(v:string)) =
                    if k="!Platform_manufacture_protocol" then
                        Some(SOFTPlatformSpecifications.ManufactureProtocol v)
                    else
                        None
                let (|CatalogNumber|_|) ((k:string),(v:string)) =
                    if k="!Platform_catalog_number" then
                        Some(SOFTPlatformSpecifications.CatalogNumber v)
                    else
                        None
                let (|WebLink|_|) ((k:string),(v:string)) =
                    if k="!Platform_web_link" then
                        Some(SOFTPlatformSpecifications.WebLink v)
                    else
                        None
                let (|Support|_|) ((k:string),(v:string)) =
                    if k="!Platform_support" then
                        Some(SOFTPlatformSpecifications.Support v)
                    else
                        None
                let (|Coating|_|) ((k:string),(v:string)) =
                    if k="!Platform_coating" then
                        Some(SOFTPlatformSpecifications.Coating v)
                    else
                        None
                let (|Description|_|) ((k:string),(v:string)) =
                    if k="!Platform_description" then
                        Some(SOFTPlatformSpecifications.Description v)
                    else
                        None
                let (|Contributor|_|) ((k:string),(v:string)) =
                    if k="!Platform_contributor" then
                        Some(SOFTPlatformSpecifications.Contributor v)
                    else
                        None
                let (|PubmedId|_|) ((k:string),(v:string)) =
                    if k="!Platform_pubmed_id" then
                        Some(SOFTPlatformSpecifications.PubmedId v)
                    else
                        None
                let (|GeoAccession|_|) ((k:string),(v:string)) =
                    if k="!Platform_geo_accession" then
                        Some(SOFTPlatformSpecifications.GeoAccession v)
                    else
                        None
                let (|TableBegin|_|) ((k:string),(v:string)) =
                    if k="!Platform_table_begin" then
                        Some(SOFTPlatformSpecifications.TableBegin v)
                    else
                        None
                let (|TableEnd|_|) ((k:string),(v:string)) =
                    if k="!Platform_table_end" then
                        Some(SOFTPlatformSpecifications.TableEnd v)
                    else
                        None

            [<RequireQualifiedAccess>]
            module Series =
                let (|Accession|_|) ((k:string),(v:string)) =
                    if k="^SERIES" then
                        Some(SOFTSeriesSpecifications.Accession v)
                    else
                        None
                let (|Title|_|) ((k:string),(v:string)) =
                    if k="!Series_title" then
                        Some(SOFTSeriesSpecifications.Title v)
                    else
                        None
                let (|Summary|_|) ((k:string),(v:string)) =
                    if k="!Series_summary" then
                        Some(SOFTSeriesSpecifications.Summary v)
                    else
                        None
                let (|OverallDesign|_|) ((k:string),(v:string)) =
                    if k="!Series_overall_design" then
                        Some(SOFTSeriesSpecifications.OverallDesign v)
                    else
                        None
                let (|PubmedId|_|) ((k:string),(v:string)) =
                    if k="!Series_pubmed_id" then
                        Some(SOFTSeriesSpecifications.PubmedId v)
                    else
                        None
                let (|WebLink|_|) ((k:string),(v:string)) =
                    if k="!Series_web_link" then
                        Some(SOFTSeriesSpecifications.WebLink v)
                    else
                        None
                let (|Contributor|_|) ((k:string),(v:string)) =
                    if k="!Series_contributor" then
                        Some(SOFTSeriesSpecifications.Contributor v)
                    else
                        None
                let (|Variable|_|) ((k:string),(v:string)) =
                
                    if k.Contains("!Series_variable_") then
                        let index =
                            k
                                .Replace("!Series_variable_","")
                                .Replace("!Series_variable_[n]","") 
                                |> int
                        Some (SOFTSeriesSpecifications.Variable(index,v))
                
                    else
                        None
                let (|VariableDescription|_|) ((k:string),(v:string)) =
                
                    if k.Contains("!Series_variable_description_") then
                        let index =
                            k
                                .Replace("!Series_variable_description_","")
                                .Replace("!Series_variable_description_[n]","") 
                                .Trim()
                                |> int
                        Some (SOFTSeriesSpecifications.VariableDescription(index,v))
                
                    else
                        None
                let (|VariableSampleList|_|) ((k:string),(v:string)) =
                
                    if k.Contains("!Series_variable_sample_list_") then
                        let index =
                            k
                                .Replace("!Series_variable_sample_list_","")
                                .Replace("!Series_variable_sample_list_[n]","") 
                                |> int
                        let keyList =
                            v.Split(',') |> Array.toList
                        Some (SOFTSeriesSpecifications.VariableSampleList(index,keyList))
                            
                    else
                        None
                let (|Repeats|_|) ((k:string),(v:string)) =
                
                    if k.Contains("!Series_repeats_") then
                        let index =
                            k
                                .Replace("!Series_repeats_","")
                                .Replace("!Series_repeats_[n]","") 
                                .Trim()
                                |> int
                        Some (SOFTSeriesSpecifications.Repeats(index,v))
                
                    else
                        None
                let (|RepeatsSampleList|_|) ((k:string),(v:string)) =
                
                    if k.Contains("!Series_repeats_sample_list_") then
                        let index =
                            k
                                .Replace("!Series_repeats_sample_list_","")
                                .Replace("!Series_repeats_sample_list_[n]","") 
                                .Trim()
                                |> int
                        let keyList =
                            v.Split(',') |> Array.toList
                        Some (SOFTSeriesSpecifications.RepeatsSampleList(index,keyList))
                            
                    else
                        None
                let (|SampleId|_|) ((k:string),(v:string)) =
                    if k="!Series_sample_id" then
                        Some(SOFTSeriesSpecifications.SampleId v)
                    else
                        None
                let (|GeoAccession|_|) ((k:string),(v:string)) =
                    if k="!Series_geo_accession" then
                        Some(SOFTSeriesSpecifications.GeoAccession v)
                    else
                        None
                let (|Type|_|) ((k:string),(v:string)) =
                    if k="!Series_type" then
                        Some(SOFTSeriesSpecifications.Type v)
                    else
                        None
                let (|SubmissionDate|_|) ((k:string),(v:string)) =
                    if k="!Series_submission_date" then
                        Some(SOFTSeriesSpecifications.SubmissionDate v)
                    else
                        None
            [<RequireQualifiedAccess>]
            module Sample =
                let (|Accession|_|) ((k:string),(v:string)) =
                    if k="^SAMPLE" then
                        Some(SOFTSampleSpecifications.Accession v)
                    else
                        None
                let (|Title|_|) ((k:string),(v:string)) =
                    if k="!Sample_title" then
                        Some(SOFTSampleSpecifications.Title v)
                    else
                        None
                let (|SupplementaryFile|_|) ((k:string),(v:string)) =
                
                    if k.Contains("!Sample_supplementary_file_") then
                        let index =
                            k
                                .Replace("!Sample_supplementary_file_","")
                                .Replace("!Sample_supplementary_file_[n]","") 
                                .Trim()
                                |> int
                        Some (SOFTSampleSpecifications.SupplementaryFile(index,v))
                
                    else
                        None
                let (|Table|_|) ((k:string),(v:string)) =
                    if k="!Sample_table" then
                        Some(SOFTSampleSpecifications.Table v)
                    else
                        None
                let (|SourceName|_|) ((k:string),(v:string)) =
                
                    if k.Contains("!Sample_source_name_") then
                        let index =
                            k
                                .Replace("!Sample_source_name_ch","")
                                .Replace("!Sample_source_name_","") 
                                .Trim()
                                |> int
                        Some (SOFTSampleSpecifications.SourceName(index,v))
                
                    else
                        None
                let (|Organism|_|) ((k:string),(v:string)) =
                
                    if k.Contains("!Sample_organism_") then
                        let index =
                            k
                                .Replace("!Sample_organism_ch","")
                                .Replace("!Sample_organism_","") 
                                .Trim()
                                |> int
                        Some (SOFTSampleSpecifications.Organism(index,v))
                
                    else
                        None
                let (|Characteristics|_|) ((k:string),(v:string)) =
                
                    if k.Contains("!Sample_characteristics_") then
                        let index =
                            k
                                .Replace("!Sample_characteristics_ch","")
                                .Replace("!Sample_characteristics_","") 
                                |> int
                        Some (SOFTSampleSpecifications.Characteristics(index,v))
                
                    else
                        None
                let (|BiomaterialProvider|_|) ((k:string),(v:string)) =
                
                    if k.Contains("!Sample_biomaterial_provider_") then
                        let index =
                            k
                                .Replace("!Sample_biomaterial_provider_ch","")
                                .Replace("!Sample_biomaterial_provider_","") 
                                .Trim()
                                |> int
                        Some (SOFTSampleSpecifications.BiomaterialProvider(index,v))
                
                    else
                        None
                let (|TreatmentProtocol|_|) ((k:string),(v:string)) =
                
                    if k.Contains("!Sample_treatment_protocol_") then
                        let index =
                            k
                                .Replace("!Sample_treatment_protocol_ch","")
                                .Replace("!Sample_treatment_protocol_","") 
                                .Trim()
                                |> int
                        Some (SOFTSampleSpecifications.TreatmentProtocol(index,v))
                
                    else
                        None
                let (|GrowthProtocol|_|) ((k:string),(v:string)) =
                
                    if k.Contains("!Sample_growth_protocol_") then
                        let index =
                            k
                                .Replace("!Sample_growth_protocol_ch","")
                                .Replace("!Sample_growth_protocol_","") 
                                .Trim()
                                |> int
                        Some (SOFTSampleSpecifications.GrowthProtocol(index,v))
                
                    else
                        None
                let (|Molecule|_|) ((k:string),(v:string)) =
                
                    if k.Contains("!Sample_molecule_") then
                        let index =
                            k
                                .Replace("!Sample_molecule_ch","")
                                .Replace("!Sample_molecule_","") 
                                |> int
                        Some (SOFTSampleSpecifications.Molecule(index,v))
                
                    else
                        None
                let (|ExtractProtocol|_|) ((k:string),(v:string)) =
                
                    if k.Contains("!Sample_extract_protocol_") then
                        let index =
                            k
                                .Replace("!Sample_extract_protocol_ch","")
                                .Replace("!Sample_extract_protocol_","") 
                                .Trim()
                                |> int
                        Some (SOFTSampleSpecifications.ExtractProtocol(index,v))
                
                    else
                        None
                let (|Label|_|) ((k:string),(v:string)) =
                
                    if k.Contains("!Sample_label_") then

                        let index =
                            k
                                .Replace("!Sample_label_ch","")
                                .Replace("!Sample_label_","") 
                                .Trim()
                                |> int
                        Some (SOFTSampleSpecifications.Label(index,v))
                
                    else
                        None
                let (|LabelProtocol|_|) ((k:string),(v:string)) =
                
                    if k.Contains("!Sample_label_protocol_") then
                        let index =
                            k
                                .Replace("!Sample_label_protocol_ch","")
                                .Replace("!Sample_label_protocol_","") 
                                .Trim()
                                |> int
                        Some (SOFTSampleSpecifications.LabelProtocol(index,v))
                
                    else
                        None
                let (|HybProtocol|_|) ((k:string),(v:string)) =
                    if k="!Sample_hyb_protocol" then
                        Some(SOFTSampleSpecifications.HybProtocol v)
                    else
                        None
                let (|ScanProtocol|_|) ((k:string),(v:string)) =
                    if k="!Sample_scan_protocol" then
                        Some(SOFTSampleSpecifications.ScanProtocol v)
                    else
                        None
                let (|DataProcessing|_|) ((k:string),(v:string)) =
                    if k="!Sample_data_processing" then
                        Some(SOFTSampleSpecifications.DataProcessing v)
                    else
                        None
                let (|Description|_|) ((k:string),(v:string)) =
                    if k="!Sample_description" then
                        Some(SOFTSampleSpecifications.Description v)
                    else
                        None
                let (|PlatformId|_|) ((k:string),(v:string)) =
                    if k="!Sample_platform_id" then
                        Some(SOFTSampleSpecifications.PlatformId v)
                    else
                        None
                let (|GeoAccession|_|) ((k:string),(v:string)) =
                    if k="!Sample_geo_accession" then
                        Some(SOFTSampleSpecifications.GeoAccession v)
                    else
                        None
                let (|Anchor|_|) ((k:string),(v:string)) =
                    if k="!Sample_anchor" then
                        Some(SOFTSampleSpecifications.Anchor v)
                    else
                        None
                let (|Type|_|) ((k:string),(v:string)) =
                    if k="!Sample_type" then
                        Some(SOFTSampleSpecifications.Type v)
                    else
                        None
                let (|TagCount|_|) ((k:string),(v:string)) =
                    if k="!Sample_tag_count" then
                        Some(SOFTSampleSpecifications.TagCount v)
                    else
                        None
                let (|TagLength|_|) ((k:string),(v:string)) =
                    if k="!Sample_tag_length" then
                        Some(SOFTSampleSpecifications.TagLength v)
                    else
                        None
                let (|TableBegin|_|) ((k:string),(v:string)) =
                    if k="!Sample_table_begin" then
                        Some(SOFTSampleSpecifications.TableBegin v)
                    else
                        None
                let (|TableEnd|_|) ((k:string),(v:string)) =
                    if k="!Sample_table_end" then
                        Some(SOFTSampleSpecifications.TableEnd v)
                    else
                        None
                let (|Relation|_|) ((k:string),(v:string)) =
                    if k="!Sample_relation" then
                        Some(SOFTSampleSpecifications.Relation v)
                    else
                        None
    
    open Generated
    open Generated.Specifications

    let private listMustContainExactlyOne failmessage l =
        match List.tryExactlyOne l with
        | Some v -> v
        | _ -> failwith failmessage

    let private listMustContainOneOrMore failmessage (l: 'a list) =
        if l.Length >= 1 then
            l
        else failwith failmessage

    type DataTable = {
        Headers: (string*string) []
        Rows: string []
    }

    type SampleRecord = {
        Accession               : string;
        Title                   : string;
        Type                    : string;
        PlatformId              : string;
        SupplementaryFile       : (int * string) list;
        Table                   : (string) list;
        SourceName              : (int * string) list;
        Organism                : (int * string) list;
        Characteristics         : (int * string) list;
        BiomaterialProvider     : (int * string) list;
        TreatmentProtocol       : (int * string) list;
        GrowthProtocol          : (int * string) list;
        Molecule                : (int * string) list;
        ExtractProtocol         : (int * string) list;
        Label                   : (int * string) list;
        LabelProtocol           : (int * string) list;
        HybProtocol             : (string) list;
        ScanProtocol            : (string) list;
        DataProcessing          : (string) list;
        Description             : (string) list;
        GeoAccession            : (string) list;
        Anchor                  : (string) list;
        TagCount                : (string) list;
        TagLength               : (string) list;
        Relation                : (string) list;
        DataTable               : DataTable
        SpecificationTokens     : SOFTSampleSpecifications list
    }
    let private createSampleRecord accession (specList:SOFTSampleSpecifications list) =
        {

            Accession = accession

            Title =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSampleSpecifications.Title v -> Some v
                        | _ -> None
                    )
                |> listMustContainExactlyOne "Title must be exactly one value"

            SupplementaryFile =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSampleSpecifications.SupplementaryFile (k,v) -> Some (k,v)
                        | _ -> None
                    )
            Table =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSampleSpecifications.Table v -> Some v
                        | _ -> None
                    )

            SourceName =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSampleSpecifications.SourceName (k,v) -> Some (k,v)
                        | _ -> None
                    )
            Organism =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSampleSpecifications.Organism (k,v) -> Some (k,v)
                        | _ -> None
                    )
        
                |> listMustContainOneOrMore "Organism must be one or more values"

            Characteristics =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSampleSpecifications.Characteristics (k,v) -> Some (k,v)
                        | _ -> None
                    )
                |> listMustContainOneOrMore "Characteristics must be one or more values"

            BiomaterialProvider =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSampleSpecifications.BiomaterialProvider (k,v) -> Some (k,v)
                        | _ -> None
                    )
            TreatmentProtocol =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSampleSpecifications.TreatmentProtocol (k,v) -> Some (k,v)
                        | _ -> None
                    )
            GrowthProtocol =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSampleSpecifications.GrowthProtocol (k,v) -> Some (k,v)
                        | _ -> None
                    )
            Molecule =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSampleSpecifications.Molecule (k,v) -> Some (k,v)
                        | _ -> None
                    )
            ExtractProtocol =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSampleSpecifications.ExtractProtocol (k,v) -> Some (k,v)
                        | _ -> None
                    )
        
                |> listMustContainOneOrMore "ExtractProtocol must be one or more values"
            
            Label =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSampleSpecifications.Label (k,v) -> Some (k,v)
                        | _ -> None
                    )
            LabelProtocol =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSampleSpecifications.LabelProtocol (k,v) -> Some (k,v)
                        | _ -> None
                    )
                

            HybProtocol =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSampleSpecifications.HybProtocol v -> Some v
                        | _ -> None
                    )


            ScanProtocol =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSampleSpecifications.ScanProtocol v -> Some v
                        | _ -> None
                    )
            DataProcessing =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSampleSpecifications.DataProcessing v -> Some v
                        | _ -> None
                    )
            Description =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSampleSpecifications.Description v -> Some v
                        | _ -> None
                    )
            PlatformId =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSampleSpecifications.PlatformId v -> Some v
                        | _ -> None
                    )
        
                |> listMustContainExactlyOne "PlatformId must be exactly one value"
            GeoAccession =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSampleSpecifications.GeoAccession v -> Some v
                        | _ -> None
                    )
            Anchor =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSampleSpecifications.Anchor v -> Some v
                        | _ -> None
                    )
            Type =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSampleSpecifications.Type v -> Some v
                        | _ -> None
                    )
                |> listMustContainExactlyOne "Type must be exactly one value"

            TagCount =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSampleSpecifications.TagCount v -> Some v
                        | _ -> None
                    )

            TagLength =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSampleSpecifications.TagLength v -> Some v
                        | _ -> None
                    )

            Relation =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSampleSpecifications.Relation v -> Some v
                        | _ -> None
                    )

            DataTable = 
                
                let headers = 
                    specList
                    |> List.choose 
                        (fun spec ->
                            match spec with
                            | SOFTSampleSpecifications.TableHeader(name,description) -> Some (name,description)
                            | _ -> None
                        )
                let rows = 
                    specList
                    |> List.choose 
                        (fun spec ->
                            match spec with
                            | SOFTSampleSpecifications.TableData v -> Some v
                            | _ -> None
                        )
                {Headers= headers |> Array.ofList; Rows = rows |> Array.ofList}

            SpecificationTokens = specList
    }

    type PlatformRecord = {
        Accession               : string;
        Title                   : string;
        Distribution            : string;
        Technology              : string;
        Organism                : string list;
        Manufacturer            : string list;
        ManufactureProtocol     : string list;
        CatalogNumber           : string list;
        WebLink                 : string list;
        Support                 : string list;
        Coating                 : string list;
        Description             : string list;
        Contributor             : string list;
        PubmedId                : string list;
        GeoAccession            : string list;
        AdditionalAttributes    : Map<string,string>
        DataTable               : DataTable
        SpecificationTokens     : SOFTPlatformSpecifications list
    }
    let private createPlatformRecord accession (specList: SOFTPlatformSpecifications list)=
        {
            Accession = accession
            Title =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTPlatformSpecifications.Title v -> Some v
                        | _ -> None
                    )
                |> listMustContainExactlyOne "Title must be exactly one value"
            
            Distribution =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTPlatformSpecifications.Distribution v -> Some v
                        | _ -> None
                    )
                |> listMustContainExactlyOne "Distribution must be exactly one value"
            
            Technology =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTPlatformSpecifications.Technology v -> Some v
                        | _ -> None
                    )
                |> listMustContainExactlyOne "Technology must be exactly one value"

            Organism =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTPlatformSpecifications.Organism v -> Some v
                        | _ -> None
                    )
                |> listMustContainOneOrMore "Organism must be one or more values"

            Manufacturer =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTPlatformSpecifications.Manufacturer v -> Some v
                        | _ -> None
                    )

            ManufactureProtocol =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTPlatformSpecifications.ManufactureProtocol v -> Some v
                        | _ -> None
                    )

            CatalogNumber =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTPlatformSpecifications.CatalogNumber v -> Some v
                        | _ -> None
                    )
            WebLink =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTPlatformSpecifications.WebLink v -> Some v
                        | _ -> None
                    )
            Support =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTPlatformSpecifications.Support v -> Some v
                        | _ -> None
                    )
            Coating =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTPlatformSpecifications.Coating v -> Some v
                        | _ -> None
                    )
            Description =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTPlatformSpecifications.Description v -> Some v
                        | _ -> None
                    )
            Contributor =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTPlatformSpecifications.Contributor v -> Some v
                        | _ -> None
                    )
            PubmedId =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTPlatformSpecifications.PubmedId v -> Some v
                        | _ -> None
                    )
            GeoAccession =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTPlatformSpecifications.GeoAccession v -> Some v
                        | _ -> None
                    )
            AdditionalAttributes =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTPlatformSpecifications.AdditionalAttribute (k,v) -> Some (k,v)
                        | _ -> None
                    )
                |> Map.ofList

            DataTable = 
                
                let headers = 
                    specList
                    |> List.choose 
                        (fun spec ->
                            match spec with
                            | SOFTPlatformSpecifications.TableHeader(name,description) -> Some (name,description)
                            | _ -> None
                        )
                let rows = 
                    specList
                    |> List.choose 
                        (fun spec ->
                            match spec with
                            | SOFTPlatformSpecifications.TableData v -> Some v
                            | _ -> None
                        )
                {Headers= headers |> Array.ofList; Rows = rows |> Array.ofList}

            SpecificationTokens = specList
    }
  
    type SeriesRecord = {
        Accession           : string;
        Title               : string;
        Summary             : (string) list;
        OverallDesign       : string list;
        PubmedId            : (string) list;
        WebLink             : (string) list;
        Contributor         : (string) list;
        Variable            : (int * string) list;
        VariableDescription : (int * string) list;
        VariableSampleList  : (int * (string list)) list;
        Repeats             : (int * string) list;
        RepeatsSampleList   : (int * (string list)) list;
        SampleId            : (string) list;
        GeoAccession        : (string) list;
        Type                : (string) list;
        SubmissionDate      : (string) list;
        SpecificationTokens : SOFTSeriesSpecifications list
    }
    let private createSeriesRecord accession (specList : SOFTSeriesSpecifications list) =
        {

            Accession =accession

            Title =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSeriesSpecifications.Title v -> Some v
                        | _ -> None
                    )
        
                |> listMustContainExactlyOne "Title must be exactly one value"
            
                

            Summary =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSeriesSpecifications.Summary v -> Some v
                        | _ -> None
                    )
        
                |> listMustContainOneOrMore "Summary must be one or more values"
            
                

            OverallDesign =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSeriesSpecifications.OverallDesign v -> Some v
                        | _ -> None
                    )

            
                

            PubmedId =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSeriesSpecifications.PubmedId v -> Some v
                        | _ -> None
                    )
        
                

            WebLink =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSeriesSpecifications.WebLink v -> Some v
                        | _ -> None
                    )
        
                

            Contributor =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSeriesSpecifications.Contributor v -> Some v
                        | _ -> None
                    )
        
                

            Variable =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSeriesSpecifications.Variable (k,v) -> Some (k,v)
                        | _ -> None
                    )
        
                

            VariableDescription =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSeriesSpecifications.VariableDescription (k,v) -> Some (k,v)
                        | _ -> None
                    )
        
                

            VariableSampleList =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSeriesSpecifications.VariableSampleList (k,v) -> Some (k,v)
                        | _ -> None
                    )
        
                

            Repeats =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSeriesSpecifications.Repeats (k,v) -> Some (k,v)
                        | _ -> None
                    )
        
                

            RepeatsSampleList =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSeriesSpecifications.RepeatsSampleList (k,v) -> Some (k,v)
                        | _ -> None
                    )
        
                

            SampleId =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSeriesSpecifications.SampleId v -> Some v
                        | _ -> None
                    )
        
                |> listMustContainOneOrMore "SampleId must be one or more values"
            
                

            GeoAccession =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSeriesSpecifications.GeoAccession v -> Some v
                        | _ -> None
                    )
        
                

            Type =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSeriesSpecifications.Type v -> Some v
                        | _ -> None
                    )
        
                

            SubmissionDate =
                specList
                |> List.choose 
                    (fun spec ->
                        match spec with
                        | SOFTSeriesSpecifications.SubmissionDate v -> Some v
                        | _ -> None
                    )

            SpecificationTokens = specList
    }

    module Tokenization =

        //Regarding to SOFT spewcifications:
        //Symbol	Description	Line type
        //^     caret lines entity indicator line
        //!     bang lines  entity attribute line
        //#     hash lines  data table header description line
        //n/a   data lines  data table row

        type SOFTToken =
        |Entity             of (string * string)
        |Attribute          of (string * string)
        |TableHeader        of (string * string)
        |TableRow           of string

        let tokenizeSOFTLine (line: string) =
            let token = 
                line.Split([|'='|])
                |> Array.map (fun s -> s.Trim())
            match token.Length with
            | 2 ->  if token.[0].StartsWith("^") then
                        Entity (token.[0], token.[1])
                    elif (token.[0].StartsWith("!")) then
                        Attribute (token.[0], token.[1])
                    elif (token.[0].StartsWith("#")) then
                        TableHeader (token.[0], token.[1])
                    else
                        TableRow (token |> String.concat "=")
            | _ ->  if token.[0].StartsWith("^") then
                        Entity (token.[0], token.[1..(token.Length-1)] |> String.concat "=")
                    elif (token.[0].StartsWith("!")) then
                        Attribute (token.[0], token.[1..(token.Length-1)] |> String.concat "=")
                    elif (token.[0].StartsWith("#")) then
                        TableHeader (token.[0], token.[1..(token.Length-1)] |> String.concat "=")
                    else
                        TableRow (token |> String.concat "=")

    module internal Parsing =

        open Tokenization
        open Generated.Lexing
        open System.Collections.Generic

        let parseSampleEntity (token:SOFTToken) (en: IEnumerator<SOFTToken>) (accession: string) =
            let rec loop (token:SOFTToken) (lexedSample:SOFTSampleSpecifications list) =
                if en.MoveNext() then
                    let nextToken = en.Current
                    match token,nextToken with
                    //gather sample infos
                    |Attribute (a,v),Attribute (nextA,nextV) 
                        -> 
                        let lexedValue =
                            match (a,v) with 
                                | Sample.Accession             lv  -> lv
                                | Sample.Relation              lv  -> lv
                                | Sample.Title                 lv  -> lv
                                | Sample.SupplementaryFile     lv  -> lv
                                | Sample.Table                 lv  -> lv
                                | Sample.SourceName            lv  -> lv
                                | Sample.Organism              lv  -> lv
                                | Sample.Characteristics       lv  -> lv
                                | Sample.BiomaterialProvider   lv  -> lv
                                | Sample.TreatmentProtocol     lv  -> lv
                                | Sample.GrowthProtocol        lv  -> lv
                                | Sample.Molecule              lv  -> lv
                                | Sample.ExtractProtocol       lv  -> lv
                                | Sample.LabelProtocol         lv  -> lv
                                | Sample.Label                 lv  -> lv
                                | Sample.HybProtocol           lv  -> lv
                                | Sample.ScanProtocol          lv  -> lv
                                | Sample.DataProcessing        lv  -> lv
                                | Sample.Description           lv  -> lv
                                | Sample.PlatformId            lv  -> lv
                                | Sample.Anchor                lv  -> lv
                                | Sample.Type                  lv  -> lv
                                | Sample.TagCount              lv  -> lv
                                | Sample.TagLength             lv  -> lv
                                | _                               -> SOFTSampleSpecifications.AdditionalAttribute (a,v)
                            
                        loop nextToken (lexedValue::lexedSample)
                            
                    | TableHeader (a,v), TableHeader (_) 
                    | TableHeader (a,v), Attribute (_)
                    | TableHeader (a,v), TableRow (_) ->

                        loop nextToken ((SOFTSampleSpecifications.TableHeader(a,v))::lexedSample)

                    | TableRow (row), TableRow (_)
                    | TableRow (row), Attribute (_) ->

                        loop nextToken ((SOFTSampleSpecifications.TableData(row))::lexedSample)

                    //return finished sample when new entity starts
                    |Attribute (a,v),Entity (e,ev) ->   
                        let lexedValue =
                            match (a,v) with 
                                | Sample.Accession             lv  -> lv
                                | Sample.Relation              lv  -> lv
                                | Sample.Title                 lv  -> lv
                                | Sample.SupplementaryFile     lv  -> lv
                                | Sample.Table                 lv  -> lv
                                | Sample.SourceName            lv  -> lv
                                | Sample.Organism              lv  -> lv
                                | Sample.Characteristics       lv  -> lv
                                | Sample.BiomaterialProvider   lv  -> lv
                                | Sample.TreatmentProtocol     lv  -> lv
                                | Sample.GrowthProtocol        lv  -> lv
                                | Sample.Molecule              lv  -> lv
                                | Sample.ExtractProtocol       lv  -> lv
                                | Sample.LabelProtocol         lv  -> lv
                                | Sample.Label                 lv  -> lv
                                | Sample.HybProtocol           lv  -> lv
                                | Sample.ScanProtocol          lv  -> lv
                                | Sample.DataProcessing        lv  -> lv
                                | Sample.Description           lv  -> lv
                                | Sample.PlatformId            lv  -> lv
                                | Sample.Anchor                lv  -> lv
                                | Sample.Type                  lv  -> lv
                                | Sample.TagCount              lv  -> lv
                                | Sample.TagLength             lv  -> lv
                                | _                               -> SOFTSampleSpecifications.AdditionalAttribute (a,v)
                            
                        nextToken , accession, createSampleRecord accession (lexedValue::lexedSample)
                    
                    |_ -> loop nextToken lexedSample
                else
                    token,accession, createSampleRecord accession lexedSample
            loop token [] 

        let parsePlatformEntity (token:SOFTToken) (en: IEnumerator<SOFTToken>) (accession: string) =
            let rec loop (token:SOFTToken) (lexedPlatform:SOFTPlatformSpecifications list) = 
                if en.MoveNext() then
                    let nextToken = en.Current
                    match token,nextToken with
                    //gather platform infos
                    | Attribute (a,v),Attribute (_) 
                    | Attribute (a,v),TableHeader (_) 
                        ->    
                        let lexedValue =
                            match (a,v) with   
                            | Platform.Accession            lv -> lv
                            | Platform.Title                lv -> lv
                            | Platform.Distribution         lv -> lv
                            | Platform.Technology           lv -> lv
                            | Platform.Organism             lv -> lv
                            | Platform.Manufacturer         lv -> lv
                            | Platform.ManufactureProtocol  lv -> lv
                            | Platform.CatalogNumber        lv -> lv
                            | Platform.WebLink              lv -> lv
                            | Platform.Support              lv -> lv
                            | Platform.Coating              lv -> lv
                            | Platform.Description          lv -> lv
                            | Platform.Contributor          lv -> lv
                            | Platform.PubmedId             lv -> lv
                            | _                                -> SOFTPlatformSpecifications.AdditionalAttribute (a,v)

                        loop nextToken (lexedValue::lexedPlatform)

                    
                    | TableHeader (a,v), TableHeader (_) 
                    | TableHeader (a,v), Attribute (_)
                    | TableHeader (a,v), TableRow (_) ->

                        loop nextToken ((SOFTPlatformSpecifications.TableHeader(a,v))::lexedPlatform)

                    | TableRow (row), TableRow (_)
                    | TableRow (row), Attribute (_) ->

                        loop nextToken ((SOFTPlatformSpecifications.TableData(row))::lexedPlatform)

                    //return finished platform when new entity starts
                    | Attribute (a,v),Entity (e,ev) ->  
                        let lexedValue =
                            match (a,v) with   
                            | Platform.Accession            lv -> lv
                            | Platform.Title                lv -> lv
                            | Platform.Distribution         lv -> lv
                            | Platform.Technology           lv -> lv
                            | Platform.Organism             lv -> lv
                            | Platform.Manufacturer         lv -> lv
                            | Platform.ManufactureProtocol  lv -> lv
                            | Platform.CatalogNumber        lv -> lv
                            | Platform.WebLink              lv -> lv
                            | Platform.Support              lv -> lv
                            | Platform.Coating              lv -> lv
                            | Platform.Description          lv -> lv
                            | Platform.Contributor          lv -> lv
                            | Platform.PubmedId             lv -> lv
                            | _                                -> SOFTPlatformSpecifications.AdditionalAttribute (a,v)
       
                        nextToken,accession,createPlatformRecord accession (lexedValue::lexedPlatform)
                    //not interesting
                    | _ -> loop nextToken lexedPlatform
                else 
                    token,accession,createPlatformRecord accession lexedPlatform
            loop token []

        let parseSeriesEntity (token:SOFTToken) (en: IEnumerator<SOFTToken>) (accession: string) =
                let rec loop (token:SOFTToken) (lexedSeries:SOFTSeriesSpecifications list) = 
                    if en.MoveNext() then
                        let nextToken = en.Current
                        match token,nextToken with
                        //gather platform infos
                        | Attribute (a,v),Attribute (nextA,nextV) ->    
                            let lexedValue =
                                match (a,v) with
                                | Series.Title                   lv -> lv
                                | Series.Summary                 lv -> lv
                                | Series.OverallDesign           lv -> lv
                                | Series.Type                    lv -> lv
                                | Series.SubmissionDate          lv -> lv
                                | Series.PubmedId                lv -> lv
                                | Series.WebLink                 lv -> lv
                                | Series.Contributor             lv -> lv
                                | Series.Variable                lv -> lv
                                | Series.VariableDescription     lv -> lv
                                | Series.VariableSampleList      lv -> lv
                                | Series.Repeats                 lv -> lv
                                | Series.RepeatsSampleList       lv -> lv
                                | Series.SampleId                lv -> lv
                                | _                                -> SOFTSeriesSpecifications.AdditionalAttribute (a,v)

                            loop nextToken (lexedValue::lexedSeries)
                        //return finished platform when new entity starts
                        | Attribute (a,v),Entity (e,ev) ->  
                            let lexedValue =
                                match (a,v) with
                                | Series.Title                   lv -> lv
                                | Series.Summary                 lv -> lv
                                | Series.OverallDesign           lv -> lv
                                | Series.Type                    lv -> lv
                                | Series.SubmissionDate          lv -> lv
                                | Series.PubmedId                lv -> lv
                                | Series.WebLink                 lv -> lv
                                | Series.Contributor             lv -> lv
                                | Series.Variable                lv -> lv
                                | Series.VariableDescription     lv -> lv
                                | Series.VariableSampleList      lv -> lv
                                | Series.Repeats                 lv -> lv
                                | Series.RepeatsSampleList       lv -> lv
                                | Series.SampleId                lv -> lv
                                | _                                -> SOFTSeriesSpecifications.AdditionalAttribute (a,v)
       
                            nextToken,accession,createSeriesRecord accession (lexedValue::lexedSeries)
                        //not interesting
                        | _ -> loop nextToken lexedSeries
                    else 
                        token,accession,createSeriesRecord accession lexedSeries
                loop token []

        let parseSOFTEntities (soft: seq<SOFTToken>) : (Map<string,SeriesRecord>)*(Map<string,SampleRecord>)*(Map<string,PlatformRecord>) =

            let en = soft.GetEnumerator()
            let rec loop (token: SOFTToken) (seriesRecords: (string*SeriesRecord) list) (sampleRecords: (string*SampleRecord) list) (platformRecords: (string*PlatformRecord) list) = 

                match token with
                |Entity (e,v) -> 
                    match (e,v) with
                    //gather Series Info
                    | Series.Accession lv ->  
                        let token', seriesAccession, seriesRecord = parseSeriesEntity token en v
                        loop (token') ((seriesAccession,seriesRecord)::seriesRecords) sampleRecords platformRecords
                    //call sample parser
                    | Sample.Accession lv ->  //printfn "SAMPLE??? %s" v
                        let token', sampleAccession, sampleRecord = parseSampleEntity token en v
                        loop (token') seriesRecords ((sampleAccession,sampleRecord)::sampleRecords) platformRecords
                    //call platform parser
                    | Platform.Accession lv -> 
                        let token', platformAccession, PlatformRecord = parsePlatformEntity token en v
                        loop (token') seriesRecords sampleRecords ((platformAccession,PlatformRecord)::platformRecords)
                    //ignore unexpected/uninteresting entities
                    | _             ->  
                        if en.MoveNext() then
                            let nextToken = en.Current
                            loop nextToken seriesRecords sampleRecords platformRecords
                        else 
                            (Map.ofList seriesRecords),(Map.ofList sampleRecords),( Map.ofList platformRecords)
                //not interesting
                |_ -> 
                    if en.MoveNext() then
                        let nextToken = en.Current
                        loop nextToken seriesRecords sampleRecords platformRecords

                        else 
                            (Map.ofList seriesRecords),(Map.ofList sampleRecords),( Map.ofList platformRecords)

            if en.MoveNext() then
                loop en.Current [] [] []
            else failwith "empty input"

    [<RequireQualifiedAccess>]
    module Series =

        open FSharpAux.IO

        type GSE = {
            SeriesMetadata      : SeriesRecord
            SampleMetadata      : Map<string,SampleRecord>
            PlatformMetadata    : Map<string,PlatformRecord>
        }

        let private createGSE (seriesRecords: Map<string,SeriesRecord>, sampleRecords: Map<string,SampleRecord>, platformRecords: Map<string,PlatformRecord>) : GSE =
            {
                SeriesMetadata =
                    seriesRecords
                    |> Map.toList
                    |> List.exactlyOne
                    |> snd

                SampleMetadata      = sampleRecords
                PlatformMetadata    = platformRecords
            }

        ///Read GEO series metadata and associated sample/platform metadata from a SOFT formatted series file (GPLXXXXX_family.soft)
        let fromFile (path:string) =
            Seq.fromFile path
            |> Seq.map Tokenization.tokenizeSOFTLine
            |> Parsing.parseSOFTEntities
            |> createGSE

        ///Read GEO series metadata and associated sample/platform metadata from a sequence of strings representing a SOFT formatted series
        let fromFileEnumerator (fileEnumerator:seq<string>) =
            fileEnumerator
            |> Seq.map Tokenization.tokenizeSOFTLine
            |> Parsing.parseSOFTEntities
            |> createGSE

        ///returns platform metadata associated with the input series GSE representation
        let getAssociatedPlatforms (gse:GSE) =
            gse.PlatformMetadata
            |> Map.toList
            |> List.map snd

        ///returns platform accessions associated with the input series GSE representation
        let getAssociatedPlatformAccessions (gse:GSE) =
            gse.PlatformMetadata
            |> Map.toList
            |> List.map fst

        ///returns sample metadata associated with the input series GSE representation
        let getAssociatedSamples (gse:GSE) =
            gse.SampleMetadata
            |> Map.toList
            |> List.map snd

        ///returns sample accessions associated with the input series GSE representation
        let getAssociatedSampleAccessions (gse:GSE) =
            gse.SampleMetadata
            |> Map.toList
            |> List.map fst


    [<RequireQualifiedAccess>]
    module Platform =

        open FSharpAux.IO

        type GPL = {
            PlatformMetadata    : PlatformRecord
            SeriesMetadata      : Map<string,SeriesRecord>
            SampleMetadata      : Map<string,SampleRecord>
        }

        let private createGPL (seriesRecords: Map<string,SeriesRecord>, sampleRecords: Map<string,SampleRecord>, platformRecords: Map<string,PlatformRecord>) : GPL =
            {
                PlatformMetadata    = 
                    platformRecords
                    |> Map.toList
                    |> List.exactlyOne
                    |> snd

                SeriesMetadata      = seriesRecords
                SampleMetadata      = sampleRecords

            }

        //Readers

        ///Read GEO platform metadata and associated sample/series Metadata from a SOFT formatted platform file (GPLXXXXX_family.soft)
        let fromFile (path:string) =
            Seq.fromFile path
            |> Seq.map Tokenization.tokenizeSOFTLine
            |> Parsing.parseSOFTEntities
            |> createGPL

        ///Read GEO platform metadata and associated sample/series Metadata from a sequence of strings representing a SOFT formatted platform
        let fromFileEnumerator (fileEnumerator:seq<string>) =
            fileEnumerator
            |> Seq.map Tokenization.tokenizeSOFTLine
            |> Parsing.parseSOFTEntities
            |> createGPL

        ///returns series metadata associated with the input platform GPL representation
        let getAssociatedSeries (gpl:GPL) =
            gpl.SeriesMetadata
            |> Map.toList
            |> List.map snd

        ///returns series accessions associated with the input platform GPL representation
        let getAssociatedSeriesAccessions (gpl:GPL) =
            gpl.SeriesMetadata
            |> Map.toList
            |> List.map snd
            |> List.map (fun record -> record.Accession)

        ///returns sample metadata associated with the input platform GPL representation
        let getAssociatedSamples (gpl:GPL) =
            gpl.SampleMetadata
            |> Map.toList
            |> List.map snd

        ///returns sample accessions associated with the input platform GPL representation
        let getAssociatedSampleAccessions (gpl:GPL) =
            gpl.SampleMetadata
            |> Map.toList
            |> List.map fst
