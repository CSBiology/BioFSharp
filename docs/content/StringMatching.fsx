(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r "../../bin/BioFSharp.dll"
#r "../../bin/BioFSharp.IO.dll"
#r "../../bin/FSharp.Care.IO.dll"
open System
open BioFSharp
open BioFSharp.IO
open FSharp.Care.IO
open AminoAcids
open Nucleotides
open AminoAcidSymbols
open BioFSharp.Algorithm.BoyerMoore
(**
The Naive Algorithm
===================
work in progress... 

Knuth Morris Pratt
==================
work in progress... 

Rabin Karp
==========
work in progress... 

Boyer Moore
===========
This part of the documentation gives a short introduction into the Boyer Moore string search algorithm which is particulary desined to work on IBioItems.This algorithm searches a query within a longer source. In using two arrays that are created using the bad character rule and the good suffix heuristics the best shifting value is determined. This accounts for higher shifts which prevents unnecessary comparisons to save time.
*)


(**
The following code will return an array created out of all the proteins that are contained in the fasta file. First, the Symbols in the file get converted 
into amino acids and then they get concatenated and transformed into an array.
*)

let chlamyproteins = 
    let converter letters : BioList.BioList<AminoAcid> = 
        letters
            |> Seq.map ( fun x -> match (BioFSharp.AminoAcids.charToParsedAminoAcidChar x) with
                                  | ParsedAminoAcidChar.StandardCodes  v -> v
                                  | ParsedAminoAcidChar.AmbiguityCodes v -> v
                                  | ParsedAminoAcidChar.GapTer         v -> v
                                  | ParsedAminoAcidChar.NoAAChar       v -> failwith "No AA Char given")
            |> Seq.toList

    FileIO.readFile (__SOURCE_DIRECTORY__ + "/data/Proteins.fasta")
    |> FastA.fromFileEnumerator (converter)
    |> Seq.map (fun item -> item.Sequence |> Seq.toArray)
    |> Seq.concat
    |> Seq.toArray


(**The following code will return the first occurence of the searched query within the source.*)
BioFSharp.Algorithm.BoyerMoore.findFirst chlamyproteins chlamyproteins.[16988..17978]

(**The following code will return the first occurence of the query after a given staring point.*)
BioFSharp.Algorithm.BoyerMoore.findFrom 7 chlamyproteins chlamyproteins.[16988..17978]

(**The following code will return the all the occurence of the query within the given source.*)
BioFSharp.Algorithm.BoyerMoore.findAll chlamyproteins chlamyproteins.[16988..17978]