(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
BioFSharp
=========

BioFSharp aims to be a user-friendly functional library for bioinformatics written in F#. It contains the basic data structures for common biological objects like amino acids and 
nucleotides based on chemical formulas and chemical elements. BioFSharp facilitate working with sequences in a strongly typed way and is designed to work well with F# Interactive. 
It provide a variety of parsers for many biological file formats and a variety of algorithms suited for bioinformatic workflows.

After you have downloaded BioFSharp, you will see how easy it is to start working with sequences:

*)

#r"BioFSharp.dll"
open BioFSharp


// Create a peptide sequence 
"PEPTIDE" |> BioSeq.ofAminoAcidString

// Create a nucleotide sequence 
"ATGC" |> BioSeq.ofNucleotideString

(**
BioFSharp comes equipped with a broad range of features and functions to map amino acids and nucleotides. 
*)
// 
Nucleotides.G |> Nucleotides.antiparallel
// Returns the monoicsotopic mass of Arginine (minus H2O)
AminoAcids.Arg |> AminoAcids.monoisoMass

(**
Analogous to the build-in collections BioFSharp provides BioSeq, BioList and BioArray for individual collection specific optimized operations. 
*)

///Peptide represented as a Bioseq
"PEPTIDE" |> BioSeq.ofAminoAcidString 

///Peptide represented as a BioList
"PEPTIDE"|> BioList.ofAminoAcidString 

///Peptide represented as a BioArray
"PEPTIDE" |> BioArray.ofAminoAcidString 


open Nucleotides

[|A;T;G;C;|].[1..3]
|> Array.map Nucleotides.antiparallel


(**
BioFSharp contains a set of readers for different biology-associated file formats like for example FastA:
*)
#r "../../bin/BioFSharp.IO.dll"
open BioFSharp.IO

let filepathFastaA = (__SOURCE_DIRECTORY__ + "/data/FASTAExample1.fasta")

//reads from file to an array of FastaItems.
FastA.fromFile BioArray.ofAminoAcidString filepathFastaA

(**
For more detailed examples continue to explore the BioFSharp documentation.
In the near future we will start to provide a cookbook like tutorial in the [CSBlog](https://csbiology.github.io/CSBlog/).
*)

