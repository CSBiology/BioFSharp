namespace BioFSharp.IO

open System
open FSharp.Care
open FSharp.Care.Collections
open FSharp.Care.IO

module FastQ =

    /// FastqItem record contains header, sequence, qualityheader, qualitysequence of one entry
    type FastqItem<'a,'b> = {
        Header    : string;
        Sequence  : 'a;
        QualityHeader : string;
        QualitySequence : 'b;       
    }
     
    /// Creates FastqItem with header line, sequence, qualityHeader and qualitySequence
    let createFastqItem header sequence qualityHeader qualitySequence =
        { Header = header; Sequence = sequence; QualityHeader = qualityHeader; QualitySequence = qualitySequence}

    /// Maps each lines from an entry to FastqItem
    let fromFileEnumerator converter qualityConverter (fileEnumerator:seq<string>) =

            // Collects all information to create FastqItem in single step by recursive memorization
            let parserSingleItem (fileEnumerator:seq<string>) =
                use en' = fileEnumerator.GetEnumerator()

                let rec parse (counter:int) header sequence qualityheader qualitysequence (en:Collections.Generic.IEnumerator<string>) =
                       en.MoveNext() |> ignore
                       match counter%4 with
                       |0 -> parse (counter+1) en.Current sequence qualityheader qualitysequence en
                       |1 -> parse (counter+1) header en.Current  qualityheader qualitysequence en
                       |2 -> parse (counter+1) header sequence en.Current   qualitysequence en
                       |3 -> createFastqItem header (converter sequence) qualityheader (qualityConverter en.Current)
                       |_ -> failwith "Unexpected line counter"
               
                parse 0 "" "" "" "" en'

            
            fileEnumerator
            |> Seq.filter (fun i -> i<>"")  // Deletes empty lines from FileEnumerator
            |> Seq.chunkBySize 4            // groups every 4 lines together 
            |> Seq.map parserSingleItem     // creates Seq of all FastqItems

    /// Reads FastqItem from FastQ format file. Converter and qualityConverter determines type of sequence by converting seq<char> -> type
    let fromFile converter qualityConverter filePath =
        FileIO.readFile filePath
        |> fromFileEnumerator converter qualityConverter

    /// Reads FastqItem from GZip format file. Converter and qualityConverter determines type of sequence by converting seq<char> -> type
    let fromGzipFile converter qualityConverter filePath =
        FileIO.readFileGZip filePath
        |> fromFileEnumerator converter qualityConverter

(*
Example of usage:

let test = @"C:\Users\User\Documents\Test.fastq"

let FastQSequence = FastQ.fromFile (fun i->i) (fun i-> i) test

Completed documentation for this module can be found on 
https://csbiology.github.io/BioFSharp/FastQ.html
*)

