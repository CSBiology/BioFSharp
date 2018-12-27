(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.

#I @"../../bin/BioFSharp/net47/"
#I @"../../bin/BioFSharp.BioDB/net45/"
#I @"../../bin/BioFSharp.ImgP/net47"
#I @"../../bin/BioFSharp.IO/net47/"
#I @"../../bin/BioFSharp.Parallel/net47/"
#I @"../../bin/BioFSharp.Stats/net47/"
#I @"../../bin/BioFSharp.Vis/net47"
#r @"../../lib/Formatting/FSharp.Plotly.dll"
#r "BioFSharp.dll"
#r "BioFSharp.IO.dll"
#r @"../..\bin\BioFSharp\net47\FSharpAux.dll"
#r @"../..\bin\BioFSharp\net47\FSharpAux.IO.dll"
open BioFSharp
open FSharpAux
open FSharpAux.IO
(**
BioFSharp
=========

BioFSharp aims to be a user-friendly functional library for bioinformatics written in F#. It contains the basic data structures for common biological objects like amino acids and nucleotides based on chemical formulas and chemical elements.

BioFSharp facilitates working with sequences in a strongly typed way and is designed to work well with F# Interactive.
It provides a variety of parsers for many biological file formats and a variety of algorithms suited for bioinformatic workflows.

The core datamodel implements in ascending hierarchical order:

- Chemical elements and [formulas](https://csbiology.github.io/BioFSharp/Formula.html) which are a collection of elements
- Amino Acids], Nucleotides and Modifications, which all implement the common [IBioItem interface](https://csbiology.github.io/BioFSharp/BioItem.html#Basics)
- [BioCollections](https://csbiology.github.io/BioFSharp/BioCollections.html) (BioItem,BioList,BioSeq) as representation of biological sequences

</br>

![Data model](https://i.imgur.com/LXBvhmi.png)

</br>

---

Installation
------------

BioFSharp is currently on the way to its 1.0.0 release. When this process is done, we will provide a nuget package at [nuget.org](https://www.nuget.org/). However, currently the way to get BioFSharp running on 
your machine is to either clone the repository and build the binaries yourself or download the prerelease packages from our [nuget branch](https://github.com/CSBiology/BioFSharp/tree/nuget).

**Using prerelease packages from the nuget branch:**

If you are using paket, add the following line to you `paket.dependencies` file:

`git https://github.com/CSBiology/BioFSharp.git nuget Packages: /`

you can then access the individual packages:

`nuget BioFSharp`

`nuget BioFSharp.IO`

`nuget BioFSharp.Stats`

`nuget BioFSharp.BioDB`

`nuget BioFSharp.Vis`


**To build the binaries yourself:**

**Windows**:

- Install [.Net Core SDK](https://www.microsoft.com/net/download)
- Install the dotnet tool fake cli by `dotnet tool install fake-cli -g` for global installation or `dotnet tool install fake-cli --tool-path yourtoolpath`
- go to the project folder
- use the console command `fake build`

**Linux(Ubuntu, using Mono)**:

- Install [.Net Core SDK](https://www.microsoft.com/net/download/linux-package-manager/ubuntu14-04/sdk-current)
- go to the project folder
- use the console command `dotnet fake build --target Linux`

</br>

---

*)


(**
Example
-------

The following example shows how easy it is to start working with sequences:
*)

// Create a peptide sequence 
let peptideSequence = "PEPTIDE" |> BioSeq.ofAminoAcidString
(***include-value:peptideSequence***)

// Create a nucleotide sequence 
let nucleotideSequence = "ATGC" |> BioSeq.ofNucleotideString
(***include-value:nucleotideSequence***)



(**
BioFSharp comes equipped with a broad range of features and functions to map amino acids and nucleotides. 
*)
// Returns the corresponding nucleotide of the anti-parallel strand
let antiG = Nucleotides.G |> Nucleotides.antiparallel
(***include-value:antiG***)

// Returns the monoicsotopic mass of Arginine (minus H2O)
let arginineMass = AminoAcids.Arg |> AminoAcids.monoisoMass
(***include-value:arginineMass***)


(**
The various file readers in BioFSharp help to easyly retrieve information and write biology-associated file formats like for example FastA:
*)
open BioFSharp.IO

let filepathFastaA = (__SOURCE_DIRECTORY__ + "/data/Chlamy_Cp.fastA")
//reads from file to an array of FastaItems.
let fastaItems = 
    FastA.fromFile BioArray.ofAminoAcidString filepathFastaA

(**
This will return a sequence of `FastaItem`s, where you can directly start working with the individual sequences represented as a `BioArray` of amino acids. 
*)

let firstItem = fastaItems |> Seq.item 0

(***include-value: firstItem***)

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

  [content]: https://github.com/CSBiology/BioFSharp/tree/master/docsrc/content
  [gh]: https://github.com/CSBiology/BioFSharp
  [issues]: https://github.com/CSBiology/BioFSharp/issues
  [readme]: https://github.com/CSBiology/BioFSharp/blob/master/README.md
  [license]: https://github.com/CSBiology/BioFSharp/blob/master/LICENSE.txt
*)