namespace BioFSharp.IO

open FSharpAux
open System.Text
open System.IO
open BioFSharp
open BioFSharp.BioID

///Contains functions for reading clustal alignment files
module Clustal = 

    //Header of file and info on conservation of sequences
    type AlignmentInfo = {Header : seq<char>; ConservationInfo : seq<char>}

    ///Reads file character by character
    let private readFile (file:string) =   
            seq {use textReader = new StreamReader(file, Encoding.Default)
                 while not textReader.EndOfStream do
                     yield textReader.Read() |> char}

    ///Tokens for lexer
    type private Token = 
        | Header of char 
        | SeqLabel of char
        | SeqEle of char
        | NextLine
    
    ///Evaluates what kind of information each character carries
    let private lexer (lastToken: Token) (symbol: char) hasReachedLength =
        match lastToken with
        | _ when symbol = '\010' || symbol = '\013'  -> NextLine
        | SeqEle _ -> SeqEle symbol
        | SeqLabel _ when hasReachedLength -> SeqEle symbol
        | SeqLabel _  -> SeqLabel symbol
        | NextLine -> SeqLabel symbol
        | Header _ -> Header symbol
    
    ///Feeds characters into lexer
    let private tokenizer (input:seq<_>) =
        let en = input.GetEnumerator()
        let rec loop labelLengthKnown i labelLength (lastToken: Token)  =
            seq {
                match en.MoveNext() with
                | false -> yield NextLine
                | true -> 
                    let labelLength',labelLengthKnown' = 
                        if labelLengthKnown then 
                            labelLength,true
                        else
                            match lastToken with
                            | SeqLabel ' ' when not (en.Current = ' ') -> i,true
                            | _ -> labelLength,false
                    let cToken =              
                        lexer lastToken en.Current ( i = labelLength')
                    yield cToken
                    yield! loop labelLengthKnown' (if cToken = NextLine then 0 else i+1) labelLength' cToken
                    }
        loop false 0 0 (Header 'p')

    ///Builds the alignment out of the Tokensequence
    let private parser (lexerInput: seq<Token>) : Alignment.Alignment<TaggedSequence<string,char>,AlignmentInfo>= 
        let sequences = System.Collections.Generic.Dictionary<string,char seq>()
        let en = lexerInput.GetEnumerator()
        let sb = System.Text.StringBuilder()
        let rec loop lastLabel lastToken seqAcc header alignment =     
            match en.MoveNext() with
            | false -> header,alignment
            | true -> 
                match en.Current with
                | SeqEle c -> 
                    match lastToken with
                    | SeqLabel _ ->
                        let label = sb.ToString().Trim()
                        sb.Clear() |> ignore
                        loop label en.Current (Seq.appendSingleton seqAcc c) header alignment
                    | _ -> 
                        loop lastLabel en.Current (Seq.appendSingleton seqAcc c) header alignment
                | SeqLabel c ->
                    sb.Append(c) |> ignore
                    loop lastLabel en.Current Seq.empty header alignment
                | NextLine -> 
                    if lastLabel = "" then 
                        loop "" en.Current Seq.empty header (Seq.append alignment seqAcc)
                    else
                        Dictionary.addOrUpdateInPlaceBy Seq.append lastLabel seqAcc sequences |> ignore
                        loop "" en.Current Seq.empty header alignment
                | Header c -> 
                    loop lastLabel en.Current seqAcc (Seq.appendSingleton header c) alignment
        let h,al = loop "" (Header 'p') Seq.empty Seq.empty Seq.empty
        {   MetaData  = {Header = h; ConservationInfo = al};
            Sequences =    
                [
                    for kv in sequences do
                        yield TaggedSequence.create kv.Key kv.Value
                ]
        }
    
    ///Reads clustal File (W or Omega) of given path and creates an alignment out of it. Also reads in numbers at end of line. Those have to be filtered out afterwards if not needed.
    let ofFile (path:string) =
        readFile path
        |> tokenizer
        |> parser
    
    ///Checks if the header of a parsed clustal alignment matches the clustal file conventions
    let hasClustalFileHeader (alignment:Alignment.Alignment<TaggedSequence<string,char>,AlignmentInfo>) = 
        let en = alignment.MetaData.Header.GetEnumerator()
        let rec loop i =
            match en.MoveNext() with
            | false -> 
                false
            | true ->      
                let symbol = en.Current
                if symbol = "CLUSTAL".[i] || symbol = "clustal".[i] then 
                    if i = 6 then 
                        true
                    else 
                        loop (i+1)
                else 
                    false     
        loop 0
    
    ///Writes an alignment to given path in clustal format. Overwrites file if it already exists
    let toFileWithOverWrite (path:string) (alignment: Alignment.Alignment<TaggedSequence<string,char>,AlignmentInfo>) =
        use sw = new System.IO.StreamWriter(path)     
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
            TaggedSequence.create "" alignment.MetaData.ConservationInfo
            |> Seq.appendSingleton alignment.Sequences 
            |> Seq.map (fun x -> 
                addEmpty x.Tag, 
                x.Sequence |> Seq.groupsOfAtMost 60 |> fun x -> x.GetEnumerator()) 
            |> Seq.toArray
        let rec loop i b =
            match b with 
            | false when i = seqs.Length ->            
                loop 0 false 
            | true when i = seqs.Length -> sw.Flush()
            | _ -> 
                let (n, s) = seqs.[i]
                match s.MoveNext() with 
                | true ->
                    if i = 0 then sw.WriteLine()
                    sw.WriteLine()
                    sw.Write(n)
                    List.iter (fun (x:char) -> sw.Write(x)) s.Current
                    
                    loop (i+1) false    
                | false -> loop (i+1) true   
        Seq.iter (fun (x:char) -> sw.Write(x)) alignment.MetaData.Header
        loop 0 false