namespace BioFSharp.IO

open System
open FSharp.Care
open FSharp.Care.Collections
open FSharp.Care.IO
    
module FastA =
            
    /// Fasta item contains header and sequence
    type FastaItem<'a> = {
        Header    : string;
        Sequence  : 'a;       
    }

            
    /// Creates with header line and sequence.
    let createFastaItem header sequence =
        { Header = header; Sequence = sequence }

        
    // Conditon of grouping lines
    let private same_group l =             
        not (String.length l = 0 || l.[0] <> '>')
    

    
    /// Reads FastaItem from file. Converter determines type of sequence by converting seq<char> -> type
    let fromFileEnumerator (converter:seq<char>-> 'a) (fileEnumerator) =
        // Matches grouped lines and concatenates them
        let record d (converter:seq<char>-> 'a) = 
            match d with
            | [] -> raise (System.Exception "Incorrect FASTA format")
            | (h:string) :: t when h.StartsWith ">" ->  let header = h .Remove(0,1)
                                                        let sequence = (Seq.concat t) |> converter
                                                        createFastaItem header sequence
                                                        
            | h :: _ -> raise (System.Exception "Incorrect FASTA format")        
    
        // main
        fileEnumerator
        |> Seq.filter (fun (l:string) -> not (l.StartsWith ";" || l.StartsWith "#"))
        |> Seq.groupWhen same_group 
        |> Seq.map (fun l -> record (List.ofSeq l) converter)
    

    /// Reads FastaItem from file. Converter determines type of sequence by converting seq<char> -> type
    let fromFile converter (filePath) =
        FileIO.readFile filePath
        |> fromFileEnumerator converter



    /// Reads FastaItem from gzFile. Converter determines type of sequence by converting seq<char> -> type
    let fromGzipFile converter (filePath) =
        FileIO.readFileGZip filePath
        |> fromFileEnumerator converter


    /// Writes FastaItem to file. Converter determines type of sequence by converting seq<char> -> type
    let write (toString:'T -> char) (filePath:string) (data:seq<FastaItem<#seq<'T>>>) =
        let toChunks (w:System.IO.StreamWriter) (length:int) (source: seq<'T>) =    
            use ie = source.GetEnumerator()
            let sourceIsEmpty = ref false
            let builder = System.Text.StringBuilder(length)
            let rec loop () =        
                    if ie.MoveNext () then                
                        builder.Append(toString ie.Current) |> ignore
                        for x in 2 .. length do
                            if ie.MoveNext() then
                                builder.Append(toString ie.Current) |> ignore
                            else
                                sourceIsEmpty := true                
                
                        match !sourceIsEmpty with
                        | false -> // writer builder
                                   w.WriteLine(builder.ToString())
                                   builder.Clear() |> ignore
                                   loop ()
                        | true  -> w.WriteLine(builder.ToString())
                                   ()
        
            loop ()
        use sWriter = new System.IO.StreamWriter(filePath,true)
        data
        |> Seq.iter (fun (i:FastaItem<_>) ->
                                sWriter.WriteLine(">" + i.Header)
                                toChunks sWriter 80 i.Sequence)   


    /// Converts FastaItem to string. Converter determines type of sequence by converting type -> seq<char>
    let toString (toString:'T -> char) (data:seq<FastaItem<#seq<'T>>>) =
        let toChunks (length:int) (source: seq<'T>) (head:string)=    
            let ie = source.GetEnumerator()
            let sourceIsEmpty = ref false
            let builder = System.Text.StringBuilder(length)        
            seq {
                yield sprintf ">%s" head
                while ie.MoveNext () do                
                            builder.Append(toString ie.Current) |> ignore
                            for x in 2 .. length do
                                if ie.MoveNext() then
                                    builder.Append(toString ie.Current) |> ignore
                                else
                                    sourceIsEmpty := true                
            
                            match !sourceIsEmpty with
                            | false -> // writer builder
                                        yield (builder.ToString())
                                        builder.Clear() |> ignore
                            | true  -> yield (builder.ToString())
                }
        data
        |> Seq.map (fun (i:FastaItem<_>) -> toChunks 80 i.Sequence i.Header)
        |> Seq.concat

