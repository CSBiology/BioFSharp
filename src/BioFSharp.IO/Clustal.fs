namespace BioFSharp.IO

open FSharp.Care.Collections
open System.Text
open System.IO
open BioFSharp.Alignment

///Contains functions for reading clustal alignment files
module Clustal = 

    ///Sequence and its ID
    type NamedSequence = {Name: string; Sequence: seq<char>}

    //Header of file and info on conservation of sequences
    type AlignmentInfo = {Header : seq<char>; ConservationInfo : seq<char>}

    ///creates NamedSequence of name and sequence
    let private createNS key value = 
        {Name = key; Sequence = value}

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
    let private parser (lexerInput: seq<Token>) : Alignment<NamedSequence,AlignmentInfo>= 
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
            AlignedSequences =    
                [
                    for kv in sequences do
                        yield createNS kv.Key kv.Value
                ]
        }
    
    ///Reads clustal File (W or Omega) of given path and creates an alignment out of it. Also reads in numbers at end of line. Those have to be filtered out afterwards if not needed.
    let ofFile (path:string) =
        readFile path
        |> tokenizer
        |> parser
    
    ///Checks if the header of a parsed clustal alignment matches the clustal file conventions
    let hasClustalFileHeader (alignment:Alignment<NamedSequence,AlignmentInfo>) = 
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