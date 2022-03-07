(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I @"../../bin/BioFSharp/net47/"
#I @"../../bin/BioFSharp.BioDB/net45/"
#I @"../../bin/BioFSharp.ImgP/net47"
#I @"../../bin/BioFSharp.IO/net47/"
#I @"../../bin/BioFSharp.Parallel/net47/"
#I @"../../bin/BioFSharp.Stats/net47/"
#I @"../../bin/BioFSharp.Vis/net47/"
#r @"../../lib/Formatting/FSharp.Plotly.dll"
#r "BioFSharp.dll"
#r "BioFSharp.IO.dll"
#r "FSharpAux.dll"
#r "FSharpAux.IO.dll"

(** 


*)
open System
open FSharpAux
open FSharpAux.IO
open BioFSharp.IO

let fileDir = __SOURCE_DIRECTORY__ + "/data/"

// http://www.bx.psu.edu/~dcking/man/maf.xhtml


/// Reads FastaItem from file. Converter determines type of sequence by converting seq<char> -> type
let fromFileEnumerator (converter:seq<char>-> 'a) (fileEnumerator) =

    // Conditon of grouping lines
    let same_group l =             
        not (String.length l = 0 || l.[0] <> 'a')

    // Matches grouped lines and concatenates them
    let record d (converter:seq<char>-> 'a) = 
        match d with
        | [] -> raise (System.Exception "Incorrect MAF format")
        | (h:string) :: l when h.StartsWith "a" ->  let header = h .Remove(0,1)
                                                    let line = (Seq.concat l) |> converter
                                                    h,l
                                                    //createFastaItem header sequence
                                                    
        | h :: _ -> raise (System.Exception "Incorrect MAF format")        

    // main
    fileEnumerator
    |> Seq.filter (fun (l:string) -> not (l.StartsWith " " || l.StartsWith "#"))
    //|> Seq.filter (fun (l:string) -> not (l.Length < 1))

    |> Seq.groupWhen same_group 
    |> Seq.map (fun l -> record (List.ofSeq l) converter)


/// Reads FastaItem from file. Converter determines type of sequence by converting seq<char> -> type
let fromFile converter (filePath) =
    FileIO.readFile filePath
    |> fromFileEnumerator converter


fromFile id (fileDir + "alignment.maf") |> Seq.length





//let rec parseS src start size strand srcSize text =
    