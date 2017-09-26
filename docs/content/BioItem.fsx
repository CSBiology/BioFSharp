(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "BioFSharp.dll"
#r "BioFSharp.IO.dll"
(**
BioItem
=========

BioFSharp includes an efficient and type save way to work with biological items like amino acids and nucleotides. 
*)

open BioFSharp


/// Returns the full name of the given amino acid 
BioItem.name AminoAcids.Arg
/// Returns the full name of the given nucleotide
BioItem.name Nucleotides.A

/// Returns the one-letter code 
BioItem.symbol AminoAcids.Arg

// Chemical formula
BioItem.formula AminoAcids.Arg



AminoAcids.isCharged AminoAcids.Arg

