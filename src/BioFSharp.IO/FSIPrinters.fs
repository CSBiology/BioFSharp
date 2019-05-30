namespace BioFSharp.IO

///Pretty printers for various custom types in the library
module FSIPrinters =

    open BioFSharp
    open BioFSharp.Alignment
    open BioFSharp.BioID
    open BioFSharp.IO
    open BioFSharp.IO.Clustal
    open BioFSharp.IO.GFF3
    open FSharpAux
    open System.Text
    
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
            let max = (Seq.maxBy (fun (x:TaggedSequence<string,char>) -> x.Tag.Length) alignment.AlignedSequences).Tag.Length
            let addEmpty (s:string) = 
                sb.Append(s) |> ignore
                for i = 0 to max - sb.Length do
                    sb.Append(' ') |> ignore
                let s = sb.ToString()
                sb.Clear() |> ignore
                s
            createTaggedSequence "" alignment.MetaData.ConservationInfo
            |> Seq.appendSingleton alignment.AlignedSequences 
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
        |> Seq.iter (fun x -> printfn "%s" x)
