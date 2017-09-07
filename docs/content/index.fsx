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
*)

#r "BioFSharp.dll"
#r "BioFSharp.IO.dll"
open BioFSharp
(**
Example
-------

The following example shows how easy it is to start working with sequences:
*)

// Create a peptide sequence 
"PEPTIDE" |> BioSeq.ofAminoAcidString
// Create a nucleotide sequence 
"ATGC" |> BioSeq.ofNucleotideString

(**
BioFSharp comes equipped with a broad range of features and functions to map amino acids and nucleotides. 
*)
// Returns the corresponding nucleotide of the anti-parallel strand
Nucleotides.G |> Nucleotides.antiparallel
// Returns the monoicsotopic mass of Arginine (minus H2O)
AminoAcids.Arg |> AminoAcids.monoisoMass




(**
Different file reader in BioFSharp help to easyly retrieve inforamtion and write  various biology-associated file formats like for example FastA:
*)
open BioFSharp.IO


let filepathFastaA = (__SOURCE_DIRECTORY__ + "/data/FASTAExample1.fasta")
//reads from file to an array of FastaItems.
FastA.fromFile BioArray.ofAminoAcidString filepathFastaA

(**
For more detailed examples continue to explore the BioFSharp documentation.
In the near future we will start to provide a cookbook like tutorial in the [CSBlog](https://csbiology.github.io/CSBlog/).
*)


(**
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][content] that can be turned into a documentation. You might
also want to read the [library design notes][readme] to understand how it works.

The library is available under Public Domain license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/CSBiology/BioFSharp/tree/master/docs/content
  [gh]: https://csbiology.github.io/BioFSharp/
  [issues]: https://github.com/CSBiology/BioFSharp/issues
  [readme]: https://github.com/CSBiology/BioFSharp/blob/master/README.md
  [license]: https://github.com/CSBiology/BioFSharp/blob/master/LICENSE.txt
*)