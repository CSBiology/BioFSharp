namespace BioFSharp.IO

module SOFT =

    open System.Collections.Generic
    open FSharpAux.IO

    //TODO : Full parsing, get rid of the "not interesting" cases and just parse everything. Remodel parser to return these types instead of string maps and such.

    type SOFTSeriesSpecifications =
    ///Provide an identifier for this entity. This identifier is used only as an internal reference within a given file. The identifier will not appear on final GEO records.
    | SERIES of string
    ///Provide a unique title that describes the overall study.
    | Series_title of string
    ///Summarize the goals and objectives of this study. The abstract from the associated publication may be suitable. You can include as much text as you need to thoroughly describe the study.
    | Series_summary of string
    ///"Provide a description of the experimental design. Indicate how many Samples are analyzed, if replicates are included, are there control and/or reference Samples, dye-swaps, etc."
    | Series_overall_design of string
    ///"Specify a valid PubMed identifier (PMID) that references a published article describing this study. Most commonly, this information is not available at the time of submission - it can be added later once the data are published."
    | Series_pubmed_id of string
    ///Specify a Web link that directs users to supplementary information about the study. Please restrict to Web sites that you know are stable.
    | Series_web_link of string
    ///"List all people associated with this study.
    | Series_contributor of string
    ///"Indicate the variable type(s) investigated in this study, e.g. !Series_variable_1 = age�!Series_variable_2 = age�"NOTE - this information is optional and does not appear in Series records or downloads, but will be used to assemble corresponding GEO DataSet records."
    | Series_variable of int * string
    ///"Describe each variable, e.g.,"!Series_variable_description_1 = 2 months !Series_variable_description_2 = 12 months"NOTE - this information is optional and does not appear in Series records or downloads, but will be used to assemble corresponding GEO DataSet records."
    | Series_variable_description of int * string
    ///"List which Samples belong to each group, e.g.,""!Series_variable_sample_list_1 = samA, samB�""!Series_variable_sample_list_2 = samC, samD�""NOTE - this information is optional and does not appear in Series records or downloads, but will be used to assemble corresponding GEO DataSet records."
    | Series_variable_sample_list of int * string
    ///"Indicate the repeat type(s), e.g.,"!Series_repeats_1 = biological replicate�!Series_repeats_2 = biological replicate"NOTE - this information is optional and does not appear in Series records or downloads, but will be used to assemble corresponding GEO DataSet records."
    | Series_repeats of int * string
    ///"List which Samples belong to each group, e.g.,�""!Series_repeats_sample_list_1 = samA, samB""!Series_repeats_sample_list_2 = samC, samD""NOTE - this information is optional and does not appear in Series
    ///records or downloads, but will be used to assemble corresponding GEO
    ////DataSet records."
    | Series_repeats_sample_list of int * string
    ///"Reference the Samples that make up this experiment. Reference the Sample accession numbers (GSMxxx) if the Samples already exists in GEO, or reference the ^Sample identifiers if they are being submitted
    ///in the same file."
    | Series_sample_id of string
    ///Only use for performing�updates�to existing GEO records.
    | Series_geo_accession of string



    type SOFTSampleSpecifications =
    ///Provide an identifier for this entity. This identifier is used only as an internal reference within a given file. The identifier will not appear on final GEO records.
    | SAMPLE of string
    ///"Provide a unique title that describes this Sample. We suggest that you use the system [biomaterial]-[condition(s)]-[replicate number], e.g., Muscle_exercised_60min_rep2."
    | Sample_title of string
    ///"Examples of supplementary file types include original Affymetrix
    ///CEL and EXP files, GenePix GPR files, and TIFF image files. Supplementary files should be zipped or tarred together with the SOFT file at
    ///time of submission (do not include any sub-directories or sub-folders in your zip/tar archive). Provision of supplementary raw data files
    ///facilitates the unambiguous interpretation of data and potential verification of conclusions as set forth in the MIAME guidelines."
    | Sample_supplementary_file of string
    ///- Affymetrix CHP file name:"If your processed data are CHP files,
    ///you can reference the CHP file name in this field. If your manuscript discusses data processed by RMA or another algorithm, we recommend providing those values in the�table section. There is no need to specify the !Sample_platform_id when CHP files are supplied. All external
    ///files should be zipped or tarred together with the SOFT file at time
    ///of submission."- Tab-delimited table file name: "If it is convenient
    ///for you to generate, you can reference the name of an external tab-delimited table file (see format) in this field, rather than include the table in the !Sample_table_begin section. All external files should be zipped or tarred together with the SOFT file at time of submission."
    | Sample_table of string
    ///"Briefly identify the biological material and the experimental variable(s), e.g., vastus lateralis muscle, exercised, 60 min."
    | Sample_source_name_ch of int * string
    ///Identify the organism(s) from which the biological material was derived.
    | Sample_organism_ch of int * string
    ///"Describe all available characteristics of the biological source,
    ///including factors not necessarily under investigation. Provide in 'Tag: Value' format, where 'Tag' is a type of characteristic (e.g. ""gender"", ""strain"", ""tissue"", ""developmental stage"", ""tumor stage"", etc), and 'Value' is the value for each tag (e.g. ""female"", ""129SV"", ""brain"", ""embryo"", etc). Include as many characteristics
    ///fields as necessary to thoroughly describe your Samples."
    | Sample_characteristics_ch of int * string
    ///"Specify the name of the company, laboratory or person that provided the biological material."
    | Sample_biomaterial_provider_ch of int * string
    ///Describe any treatments applied to the biological material prior to extract preparation. You can include as much text as you need to thoroughly describe the protocol; it is strongly recommended that complete protocol descriptions are provided within your submission.
    | Sample_treatment_protocol_ch of int * string
    ///Describe the conditions that were used to grow or maintain organisms or cells prior to extract preparation. You can include as much text as you need to thoroughly describe the protocol; it is strongly recommended that complete protocol descriptions are provided within your submission.
    | Sample_growth_protocol_ch of int * string
    ///Specify the type of molecule that was extracted from the biological material.
    | Sample_molecule_ch of int * string
    ///Describe the protocol used to isolate the extract material. You can include as much text as you need to thoroughly describe the protocol; it is strongly recommended that complete protocol descriptions are provided within your submission.
    | Sample_extract_protocol_ch of int * string
    ///"Specify the compound used to label the extract e.g., biotin, Cy3, Cy5, 33P."
    | Sample_label_ch of int * string
    ///Describe the protocol used to label the extract. You can include as much text as you need to thoroughly describe the protocol; it is strongly recommended that complete protocol descriptions are provided within your submission.
    | Sample_label_protocol_ch of int * string
    ///"Describe the protocols used for hybridization, blocking and washing, and any post-processing steps such as staining. You can include as much text as you need to thoroughly describe the protocol; it is strongly recommended that complete protocol descriptions are provided within your submission."
    | Sample_hyb_protocol of string
    ///"Describe the scanning and image acquisition protocols, hardware,
    ///and software. You can include as much text as you need to thoroughly
    ///describe the protocol; it is strongly recommended that complete protocol descriptions are provided within your submission."
    | Sample_scan_protocol of string
    ///"Provide details of how data in the VALUE column of your table were generated and calculated, i.e., normalization method, data selection procedures and parameters, transformation algorithm (e.g., MAS5.0), and scaling parameters. You can include as much text as you need to
    ///thoroughly describe the processing procedures."
    | Sample_data_processing of string
    ///"Include any additional information not provided in the other fields, or paste in broad descriptions that cannot be easily dissected into the other fields."
    | Sample_description of string
    ///"Reference the Platform upon which this hybridization was performed. Reference the Platform accession number (GPLxxx) if the Platform already exists in GEO, or reference the ^Platform identifier if the Platform record is being batch submitted within the same SOFT file. To
    ///identify the accession number of an existing commercial Platform in GEO, use the�FIND PLATFORM�tool."
    | Sample_platform_id of string
    ///Only use for performing�updates�to existing GEO records.
    | Sample_geo_accession of string
    ///Use for SAGE submissions only.
    | Sample_anchor of string
    ///Use for SAGE submissions only.
    | Sample_type of string
    ///Use for SAGE submissions only.
    | Sample_tag_count of string
    ///Use for SAGE submissions only.
    | Sample_tag_length of string
    ///Indicates the start of the data table.
    | Sample_table_begin of string
    ///Indicates the end of the data table.
    | Sample_table_end of string

    ///Type Representation of SOFT `^Platforms` Entities
    type PlatformInfos = {
        Accession: string
        Name: string
        Organism:string
    }

    let private createPlatformInfos acc name org = {Accession = acc; Name = name; Organism= org}

    type SampleInfos = {
        Accession: string
        Title : string
        Channels: int
        Organism: Map<string,string list>
        Source:Map<string,string list>
        Characteristics: Map<string,string list>
        Molecules: Map<string,string list>
        SRAAccession: string
    }

    let private createSampleInfos acc t ch org src char mol sra = 
        {
            Accession       = acc
            Title           = t
            Channels        = ch
            Organism        = org
            Source          = src
            Characteristics = char
            Molecules       = mol
            SRAAccession    = sra
        }

    type SeriesInfos = {
        Accession : string
        Title: string
        Design: string
        Type : string list
        SampleIds: string list
        Date: string
        Publications: string list
        Platform : Map<string,PlatformInfos>
        SampleAnnotations: Map<string,SampleInfos>
    }

    let private createSeriesInfo acc t des typ sIds d pub pI sI =
        {
            Accession = acc
            Title = t
            Design = des
            Type = typ
            SampleIds = sIds
            Date = d
            Publications = pub
            Platform = pI
            SampleAnnotations = sI
        }

    type SOFTToken =
    |Entity of (string * string)
    |Attribute of (string * string)
    |DataTable
    |Broken of string []

    let private tokenizeSOFTLine (line: string) =
        let token = 
            line.Split([|'='|])
            |> Array.map (fun s -> s.Trim())
        match token.Length with
        | 1 -> DataTable
        | 2 ->  if token.[0].StartsWith("^") then
                    Entity (token.[0], token.[1])
                elif (token.[0].StartsWith("!")) then
                    Attribute (token.[0], token.[1])
                else
                    Broken token
        | _ ->  if token.[0].StartsWith("^") then
                    Entity (token.[0], token.[1..(token.Length-1)] |> String.concat "=")
                elif (token.[0].StartsWith("!")) then
                    Attribute (token.[0], token.[1..(token.Length-1)] |> String.concat "=")
                else
                    Broken token

    let private reduceMaps (input: (string*string) list) =
        input
        |> List.groupBy (fun (k,v) -> k)
        |> List.map (fun (k,v) -> k, v |> List.map (fun (k',v') -> v'))


    let private parseSample (token:SOFTToken) (en: IEnumerator<SOFTToken>) (accession: string) =
        let rec loop (token:SOFTToken) (acc: string) (t : string) (ch: int) (org: (string*string) list) (src:(string*string) list) (char: (string*string) list) (mol: (string*string) list) (sra:string) =
            if en.MoveNext() then
                let nextToken = en.Current
                match token,nextToken with
                //gather sample infos
                |Attribute (a,v),Attribute (nextA,nextV) -> match a with
                                                            | "!Sample_title"                                   -> loop nextToken acc v ch org src char mol sra
                                                            | "!Sample_channel_count"                           -> loop nextToken acc t (int v) org src char mol sra
                                                            | a when a.Contains("!Sample_source_name_ch")       -> loop nextToken acc t ch org ((a,v)::src) char mol sra
                                                            | a when a.Contains("!Sample_organism_ch")          -> loop nextToken acc t ch ((a,v)::org) src char mol sra
                                                            | a when a.Contains("!Sample_characteristics_ch")   -> loop nextToken acc t ch org src ((a,v)::char) mol sra
                                                            | a when a.Contains("!Sample_molecule_ch")          -> loop nextToken acc t ch org src char ((a,v)::mol) sra
                                                            | "!Sample_relation" when (v.Contains("SRA"))       -> loop nextToken acc t ch org src char mol (if v.Contains("SRA") then v else sra)
                                                            | _                                                 -> loop nextToken acc t ch org src char mol sra
                            
                //return finished sample when new entity starts
                |Attribute (a,v),Entity (e,ev) ->   match a with
                                                    | "!Sample_title"                                   -> nextToken,acc,(createSampleInfos acc v ch (Map.ofList (reduceMaps org)) (Map.ofList (reduceMaps src)) (Map.ofList (reduceMaps char)) (Map.ofList (reduceMaps mol)) sra)
                                                    | "!Sample_channel_count"                           -> nextToken,acc,(createSampleInfos acc t (int v) (Map.ofList (reduceMaps org)) (Map.ofList (reduceMaps src)) (Map.ofList (reduceMaps char)) (Map.ofList (reduceMaps mol)) sra)
                                                    | a when a.Contains("!Sample_source_name_ch")       -> nextToken,acc,(createSampleInfos acc t ch (Map.ofList (reduceMaps org)) (Map.ofList (reduceMaps ((a,v)::src))) (Map.ofList (reduceMaps char)) (Map.ofList (reduceMaps mol)) sra)
                                                    | a when a.Contains("!Sample_organism_ch")          -> nextToken,acc,(createSampleInfos acc t ch (Map.ofList (reduceMaps ((a,v)::org))) (Map.ofList (reduceMaps src)) (Map.ofList (reduceMaps char)) (Map.ofList (reduceMaps mol)) sra)
                                                    | a when a.Contains("!Sample_characteristics_ch")   -> nextToken,acc,(createSampleInfos acc t ch (Map.ofList (reduceMaps org)) (Map.ofList (reduceMaps src)) (Map.ofList (reduceMaps ((a,v)::char))) (Map.ofList (reduceMaps mol)) sra)
                                                    | a when a.Contains("!Sample_molecule_ch")          -> nextToken,acc,(createSampleInfos acc t ch (Map.ofList (reduceMaps org)) (Map.ofList (reduceMaps src)) (Map.ofList (reduceMaps char)) (Map.ofList (reduceMaps ((a,v)::mol))) sra)
                                                    | "!Sample_relation"                                -> nextToken,acc,(createSampleInfos acc t ch (Map.ofList (reduceMaps org)) (Map.ofList (reduceMaps src)) (Map.ofList (reduceMaps char)) (Map.ofList (reduceMaps mol)) (if v.Contains("SRA") then v else sra))
                                                    | _                                                 -> nextToken,acc,(createSampleInfos acc t ch (Map.ofList (reduceMaps org)) (Map.ofList (reduceMaps src)) (Map.ofList (reduceMaps char)) (Map.ofList (reduceMaps mol)) sra)
                //not interesting
                |_ -> loop nextToken acc t ch org src char mol sra
            else 
                token,acc,(createSampleInfos acc t ch (Map.ofList (reduceMaps org)) (Map.ofList (reduceMaps src)) (Map.ofList (reduceMaps char)) (Map.ofList (reduceMaps mol)) sra)
        loop token accession "" -1 [] [] [] [] ""

    let private parsePlatformInfos (token:SOFTToken) (en: IEnumerator<SOFTToken>) (accession: string) =
        let rec loop (token:SOFTToken) acc name org = 
            if en.MoveNext() then
                let nextToken = en.Current
                match token,nextToken with
                //gather platform infos
                | Attribute (a,v),Attribute (nextA,nextV) ->    match a with   
                                                                | "!Platform_title" -> loop nextToken acc v org
                                                                | "!Platform_organism" -> loop nextToken acc name v
                                                                | _ -> loop nextToken acc name org
                //return finished platform when new entity starts
                | Attribute (a,v),Entity (e,ev) ->  match a with   
                                                    | "!Platform_title" -> token,acc,createPlatformInfos acc v org
                                                    | "!Platform_organism" -> token,acc,createPlatformInfos acc name org
                                                    | _ -> nextToken,acc,createPlatformInfos acc name org
                //not interesting
                | _ -> loop nextToken acc name org
            else 
                token,acc,createPlatformInfos acc name org
        loop token accession ""  ""

    let private parseSOFT (soft: seq<SOFTToken>) =
        let en = soft.GetEnumerator()

        let rec loop (token: SOFTToken) (seriesAccession: string) (seriesTitle: string) (seriesDesign: string) (seriesType:string list) (seriesSamples:string list) (date: string) (publications: string list) (samples: (string*SampleInfos) list) (platform: (string*PlatformInfos) list) = 

            match token with
            |Entity (e,v) -> 
                match e with
                //gather Series Info
                | "^SERIES"     ->  
                    if en.MoveNext() then
                        let nextToken = en.Current
                        loop nextToken (v) seriesTitle seriesDesign seriesType seriesSamples date publications samples platform
                    else 
                        createSeriesInfo seriesAccession seriesTitle seriesDesign seriesType seriesSamples date publications (Map.ofList platform) ( Map.ofList samples)
                //call sample parser
                | "^SAMPLE"     ->  //printfn "SAMPLE??? %s" v
                                    let token', smplAcc, smpl = parseSample token en v
                                    loop (token') seriesAccession seriesTitle seriesDesign seriesType seriesSamples date publications ((smplAcc,smpl)::samples) platform
                //call platform parser
                | "^PLATFORM"   ->  let token', pltfrmlAcc, pltfrm = parsePlatformInfos token en v
                                    loop (token') seriesAccession seriesTitle seriesDesign seriesType seriesSamples date publications samples ((pltfrmlAcc,pltfrm)::platform)
                //ignore unexpected/uninteresting entities
                | _             ->  
                    if en.MoveNext() then
                        let nextToken = en.Current
                        loop nextToken seriesAccession seriesTitle seriesDesign seriesType seriesSamples date publications samples platform
                    else 
                        createSeriesInfo seriesAccession seriesTitle seriesDesign seriesType seriesSamples date publications (Map.ofList platform) ( Map.ofList samples)
            //Attributes are only found here when on series level or uninteresting
            |Attribute (a,v) -> match a with
                                | "!Series_title"                           -> 
                                    if en.MoveNext() then
                                        let nextToken = en.Current
                                        loop nextToken seriesAccession (v) seriesDesign seriesType seriesSamples date publications samples platform
                                    else 
                                        createSeriesInfo seriesAccession seriesTitle seriesDesign seriesType seriesSamples date publications (Map.ofList platform) ( Map.ofList samples)
                                | "!Series_submission_date"                 -> 
                                    if en.MoveNext() then
                                        let nextToken = en.Current
                                        loop nextToken seriesAccession seriesTitle seriesDesign seriesType seriesSamples (v) publications samples platform
                                    else 
                                        createSeriesInfo seriesAccession seriesTitle seriesDesign seriesType seriesSamples date publications (Map.ofList platform) ( Map.ofList samples)
                                | "!Series_pubmed_id" | "!Series_web_link"  -> 
                                    if en.MoveNext() then
                                        let nextToken = en.Current
                                        loop nextToken seriesAccession seriesTitle seriesDesign seriesType seriesSamples date ((sprintf "%s = %s" a v)::publications) samples platform
                                    else 
                                        createSeriesInfo seriesAccession seriesTitle seriesDesign seriesType seriesSamples date publications (Map.ofList platform) ( Map.ofList samples)
                                | "!Series_overall_design"                  -> 
                                    if en.MoveNext() then
                                        let nextToken = en.Current
                                        loop nextToken seriesAccession seriesTitle v seriesType seriesSamples date publications samples platform
                                    else 
                                        createSeriesInfo seriesAccession seriesTitle seriesDesign seriesType seriesSamples date publications (Map.ofList platform) ( Map.ofList samples)
                                | "!Series_type"                            -> 
                                    if en.MoveNext() then
                                        let nextToken = en.Current
                                        loop nextToken seriesAccession seriesTitle seriesDesign (v::seriesType) seriesSamples date publications samples platform
                                    else 
                                        createSeriesInfo seriesAccession seriesTitle seriesDesign seriesType seriesSamples date publications (Map.ofList platform) ( Map.ofList samples)
                                | "!Series_sample_id"                       -> 
                                    if en.MoveNext() then
                                        let nextToken = en.Current
                                        loop nextToken seriesAccession seriesTitle seriesDesign seriesType (v::seriesSamples) date  publications samples platform
                                    else 
                                        createSeriesInfo seriesAccession seriesTitle seriesDesign seriesType seriesSamples date publications (Map.ofList platform) ( Map.ofList samples)
                                //attributes of uninteresting entities
                                | _ -> 
                                    if en.MoveNext() then
                                        let nextToken = en.Current
                                        loop nextToken seriesAccession seriesTitle seriesDesign seriesType seriesSamples date publications samples platform
                                    else 
                                        createSeriesInfo seriesAccession seriesTitle seriesDesign seriesType seriesSamples date publications (Map.ofList platform) ( Map.ofList samples)
            //not interesting
            |_ -> 
                if en.MoveNext() then
                    let nextToken = en.Current
                    loop nextToken seriesAccession seriesTitle seriesDesign seriesType seriesSamples date publications samples platform
                else 
                    createSeriesInfo seriesAccession seriesTitle seriesDesign seriesType seriesSamples date publications (Map.ofList platform) ( Map.ofList samples)

        if en.MoveNext() then
            loop en.Current "" "" "" [] [] "" [] [] []
        else failwith "empty input"

    ///Read SOFT SeriesInfo from a SOFT series file
    let fromSeriesFile (path:string) =
        Seq.fromFile path
        |> Seq.map tokenizeSOFTLine
        |> parseSOFT

    ///Read SOFT SeriesInfo from a string representing a SOFT Series file
    let fromSeriesFileEnumerator (fileEnumerator:seq<string>) =
        fileEnumerator
        |> Seq.map tokenizeSOFTLine
        |> parseSOFT