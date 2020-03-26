namespace BioFSharp.IO

///Pretty printers for various custom types in the library
module FSIPrinters =

    open BioFSharp
    open BioFSharp.Alignment
    open BioFSharp.BioID
    open BioFSharp.TaggedSequence
    open BioFSharp.IO
    open BioFSharp.IO.Clustal
    open BioFSharp.IO.GFF3
    open FSharpAux
    open System.Text

    module internal FormatHelpers =
        
            module SOFT =

                let formatSingleEntry rootIdent (s: string) =
                    let ident = [for i in [1 .. (4*rootIdent)] do yield " "] |> String.concat ""
                    s
                    |> String.split ' '
                    |> Array.chunkBySize 15
                    |> Array.map (String.concat " ")
                    |> Array.mapi (fun i s -> if i = 0 then s else sprintf "%s%s" ident s)
                    |> String.concat "\r\n"

                let formatMultiEntries rootIdent (l: string list) =
                    let ident = [for i in [1 .. (4*rootIdent)] do yield " "] |> String.concat ""
                    l 
                    |> List.map 
                        (fun summary -> 
                            summary
                            |> String.split ' '
                            |> Array.chunkBySize 15
                            |> Array.map (String.concat " ")
                        )
                    |> Array.concat
                    |> Array.mapi (fun i s -> if i = 0 then s else sprintf "%s%s" ident s)
                    |> String.concat "\r\n"

                let formatSamples rootIdent (sm : Map<string,SampleRecord>) = 
                    let ident = [for i in [1 .. (4*rootIdent)] do yield " "] |> String.concat "" 
                    sm
                    |> Map.toList
                    |> List.map 
                        (fun (k,v) ->
                            sprintf "%s => %s" k v.Title
                        )
                    |> List.mapi (fun i s -> if i = 0 then s else sprintf "%s%s" ident s)
                    |> fun lines ->
                        if lines.Length > 15 then
                            let morelineCount = lines.Length - 15
                            [
                                yield! lines.[0..14]
                                sprintf "%s(...and %i more)" ident morelineCount
                            ]
                            |> String.concat "\r\n"
                        else
                            lines |> String.concat "\r\n"
    
                let formatPlatforms rootIdent (sm : Map<string,PlatformRecord>) = 
                    let ident = [for i in [1 .. (4*rootIdent)] do yield " "] |> String.concat ""
                    sm
                    |> Map.toList
                    |> List.map 
                        (fun (k,v) ->
                            sprintf "%s => %s" k v.Title
                        )
                    |> List.mapi (fun i s -> if i = 0 then s else sprintf "%s%s" ident s)
                    |> fun lines ->
                        if lines.Length > 15 then
                            let morelineCount = lines.Length - 15
                            [
                                yield! lines.[0..14]
                                sprintf "%s(...and %i more)" ident morelineCount
                            ]
                            |> String.concat "\r\n"
                        else
                            lines |> String.concat "\r\n"

                let formatSeries rootIdent (sm : Map<string,SeriesRecord>) = 
                    let ident = [for i in [1 .. (4*rootIdent)] do yield " "] |> String.concat ""
                    sm
                    |> Map.toList
                    |> List.map 
                        (fun (k,v) ->
                            sprintf "%s => %s" k v.Title
                        )
                    |> List.mapi (fun i s -> if i = 0 then s else sprintf "%s%s" ident s)
                    |> fun lines ->
                        if lines.Length > 15 then
                            let morelineCount = lines.Length - 15
                            [
                                yield! lines.[0..14]
                                sprintf "%s(...and %i more)" ident morelineCount
                            ]
                            |> String.concat "\r\n"
                        else
                            lines |> String.concat "\r\n"

                let formatMultiChannelEntry rootIdent l =
                    l 
                    |> List.groupBy fst 
                    |> List.map (fun (i,c) -> (sprintf "[CHANNEL %i]\r\n" i ) :: (c |> List.map snd)) 
                    |> List.concat 
                    |> formatMultiEntries rootIdent

    open FormatHelpers

    ///print BioItems by using symbols for AminoAcids and Nucleotides, and the name of Modifications in [brackets]
    let prettyPrintBioItem (a: 'a when 'a :> IBioItem) =
        match (a :> IBioItem) with
        | :? AminoAcids.AminoAcid | :? Nucleotides.Nucleotide-> sprintf "%c" (BioItem.symbol a)
        | :? ModificationInfo.Modification -> sprintf "[%s]" (BioItem.name a)
        | _ -> "?"
    
    ///print BioItems by using symbols for AminoAcids and Nucleotides, and the name of Modifications in [brackets]
    let prettyPrintBioItemWithModifications (a: 'a when 'a :> IBioItem) =
        match (a :> IBioItem) with
        | :? Nucleotides.Nucleotide as n -> sprintf "%c" (BioItem.symbol n)
        | :? ModificationInfo.Modification as m -> sprintf "[%s]" (BioItem.name m)
        | :? AminoAcids.AminoAcid as aa ->
            match aa with
            | AminoAcids.AminoAcid.Mod (aa',m) -> sprintf "%c[%s]" (BioItem.symbol aa') (m |> Seq.map (fun md -> md.Name) |> String.concat ";")
            | _ -> sprintf "%c" (BioItem.symbol aa)
        | _ -> "?"

    ///print Biocollections in 6x10char blocks per line, preceeded by an index indicator
    let prettyPrintBioCollection (sequence:seq<'a> when 'a :> IBioItem ) =
        let stringSplits =
            sequence 
            |> Seq.chunkBySize 60
        let innerStringSplits =
            stringSplits
            |> Seq.map (Seq.chunkBySize 10)
        innerStringSplits 
        |> Seq.mapi (fun i strs ->  let lineIndex = (i * 60 + 1)
                                    let line = 
                                        strs 
                                        |> Seq.fold (fun acc elem -> sprintf "%s %s" acc (elem |> Seq.map prettyPrintBioItem |> String.concat "")) "" 
                                    sprintf "%s%i %s" ([for x in 1 .. (10 - (string lineIndex).Length) do yield " "] |> String.concat "") lineIndex line )
        |> String.concat "\r\n"
        |> (fun prnt -> sprintf "\r\n%s\r\n" prnt)

    ///print Biocollections in 6x10char blocks per line, preceeded by an index indicator
    let prettyPrintBioCollectionWithModifications (sequence:seq<'a> when 'a :> IBioItem ) =
        let stringSplits =
            sequence 
            |> Seq.chunkBySize 60
        let innerStringSplits =
            stringSplits
            |> Seq.map (Seq.chunkBySize 10)
        innerStringSplits 
        |> Seq.mapi (fun i strs ->  let lineIndex = (i * 60 + 1)
                                    let line = 
                                        strs 
                                        |> Seq.fold (fun acc elem -> sprintf "%s %s" acc (elem |> Seq.map prettyPrintBioItemWithModifications |> String.concat "")) "" 
                                    sprintf "%s%i %s" ([for x in 1 .. (10 - (string lineIndex).Length) do yield " "] |> String.concat "") lineIndex line )
        |> String.concat "\r\n"
        |> (fun prnt -> sprintf "\r\n%s\r\n" prnt)

    ///print Clustal formatted file as seen in the specifications.
    let prettyPrintClustal (alignment: Alignment<TaggedSequence<string,char>,AlignmentInfo>) =
        let prnt = new StringBuilder()
        let seqs = 
            let sb = StringBuilder()
            let max = (Seq.maxBy (fun (x:TaggedSequence<string,char>) -> x.Tag.Length) alignment.Sequences).Tag.Length
            let addEmpty (s:string) = 
                sb.Append(s) |> ignore
                for i = 0 to max - sb.Length do
                    sb.Append(' ') |> ignore
                let s = sb.ToString()
                sb.Clear() |> ignore
                s
            createTaggedSequence "" alignment.MetaData.ConservationInfo
            |> Seq.appendSingleton alignment.Sequences 
            |> Seq.map (fun x -> 
                addEmpty x.Tag, 
                x.Sequence |> Seq.groupsOfAtMost 60 |> fun x -> x.GetEnumerator()) 
            |> Seq.toArray
        let rec loop i b =
            match b with 
            | false when i = seqs.Length ->            
                loop 0 false 
            | true when i = seqs.Length -> ()
            | _ -> 
                let (n, s) = seqs.[i]
                match s.MoveNext() with 
                | true ->
                    if i = 0 then prnt.AppendLine() |> ignore
                    prnt.AppendLine() |> ignore
                    prnt.AppendLine(n) |> ignore
                    List.iter (fun (x:char) -> prnt.Append(x) |> ignore) s.Current
                    loop (i+1) false    
                | false -> loop (i+1) true   
        Seq.iter (fun (x:char) -> prnt.Append(x) |> ignore) alignment.MetaData.Header
        loop 0 false
        sprintf "\r\n%s\r\n" (prnt.ToString())

    ///print GFF3 formatted file as seen in the specifications.
    let prettyPrintGFF3 (input : seq<GFFLine<#seq<'a>>>) =
        toString id input
        |> String.concat "\r\n"

    let prettyPrintSampleRecord (sample:SOFT.SampleRecord) =
        sprintf
            """
METADATA FOR SAMPLE RECORD %s
===========================%s

Title:              %s

Type(s):            %s

Organism(s)         %s

Characteristics:    %s

Description:        %s

(For more Metadata, access the type directly)
            """
            sample.Accession
            (sample.Accession           |> String.map (fun c -> '='))
            (sample.Title               |> SOFT.formatSingleEntry 5)
            (sample.Type                |> SOFT.formatSingleEntry 5)
            (sample.Organism            |> SOFT.formatMultiChannelEntry 5)
            (sample.Characteristics     |> SOFT.formatMultiChannelEntry 5)
            (sample.Description         |> SOFT.formatMultiEntries 5)

    let prettyPrintSeriesRecord (series:SOFT.SeriesRecord) =
        sprintf
            """
GEO SERIES RECORD %s
==================%s

Type(s):        %s

Title:          %s

Contributor(s): %s

Design:         %s

Summary:        %s

(For more Metadata, access the type directly)
            """
            series.Accession
            (series.Accession           |> String.map (fun c -> '='))
            (series.Type                |> SOFT.formatMultiEntries 4)
            (series.Title               |> SOFT.formatSingleEntry 4)
            (series.Contributor         |> SOFT.formatMultiEntries 4)
            (series.OverallDesign       |> SOFT.formatMultiEntries 4)
            (series.Summary             |> SOFT.formatMultiEntries 4)

    let prettyPrintPlatformRecord (platform:SOFT.PlatformRecord) =
        sprintf
            """
GEO PLATFORM RECORD %s
====================%s

Title:          %s

Organism(s):    %s

Description:    %s

Technology:     %s

Contributor(s): %s

(For more Metadata, access the type directly)
            """
            platform.Accession
            (platform.Accession             |> String.map (fun c -> '='))
            (platform.Title                 |> SOFT.formatSingleEntry 4)
            (platform.Organism              |> SOFT.formatMultiEntries 4)
            (platform.Description           |> SOFT.formatMultiEntries 4)
            (platform.Technology            |> SOFT.formatSingleEntry 4)
            (platform.Contributor           |> SOFT.formatMultiEntries 4)

    let prettyPrintGSE (gse:SOFT.Series.GSE) =
        sprintf
            """
GEO SERIES RECORD %s
==================%s

Type(s):        %s

Platform(s):    %s

Title:          %s

Contributor(s): %s

Design:         %s

Summary:        %s

Samples         %s

(For more Metadata, access the type directly)
            """
            gse.SeriesMetadata.Accession
            (gse.SeriesMetadata.Accession           |> String.map (fun c -> '='))
            (gse.SeriesMetadata.Type                |> SOFT.formatMultiEntries 4)
            (gse.PlatformMetadata                   |> SOFT.formatPlatforms 4)
            (gse.SeriesMetadata.Title               |> SOFT.formatSingleEntry 4)
            (gse.SeriesMetadata.Contributor         |> SOFT.formatMultiEntries 4)
            (gse.SeriesMetadata.OverallDesign       |> SOFT.formatMultiEntries 4)
            (gse.SeriesMetadata.Summary             |> SOFT.formatMultiEntries 4)
            (gse.SampleMetadata                     |> SOFT.formatSamples 4)

    let prettyPrintGPL (gpl:SOFT.Platform.GPL) =
        sprintf
            """
GEO PLATFORM RECORD %s
====================%s

Title:          %s

Organism(s):    %s

Description:    %s

Technology:     %s

Contributor(s): %s

Series(s):      %s

Samples         %s

(For more Metadata, access the type directly)
            """
            gpl.PlatformMetadata.Accession
            (gpl.PlatformMetadata.Accession             |> String.map (fun c -> '='))
            (gpl.PlatformMetadata.Title                 |> SOFT.formatSingleEntry 4)
            (gpl.PlatformMetadata.Organism              |> SOFT.formatMultiEntries 4)
            (gpl.PlatformMetadata.Description           |> SOFT.formatMultiEntries 4)
            (gpl.PlatformMetadata.Technology            |> SOFT.formatSingleEntry 4)
            (gpl.PlatformMetadata.Contributor           |> SOFT.formatMultiEntries 4)
            (gpl.SeriesMetadata                         |> SOFT.formatSeries 4)
            (gpl.SampleMetadata                         |> SOFT.formatSamples 4)

