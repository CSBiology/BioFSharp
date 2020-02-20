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

**From Nuget.org:**

You can get the stable versions of all BioFSharp packages from nuget:

<pre>
Install-Package BioFSharp
paket add BioFSharp
</pre>

All associated packages can be found [here](https://www.nuget.org/profiles/CSBiology)


**Prerelease packages from the nuget branch:**

Unstable/Experimental packages only.

If you are using paket, add the following line to you `paket.dependencies` file:

`git https://github.com/CSBiology/BioFSharp.git nuget Packages: /`

you can then access the individual packages:

`nuget BioFSharp`

`nuget BioFSharp.BioContainers`

`nuget BioFSharp.IO`

`nuget BioFSharp.Stats`

`nuget BioFSharp.ML`

`nuget BioFSharp.BioDB`

`nuget BioFSharp.Vis`


**Build the binaries yourself:**

**Windows**:

- Install [.Net Core SDK](https://www.microsoft.com/net/download) 3.0 +
- go to the project folder
- `.\build.cmd`

**Linux(using Mono)**:

- BioDB is excluded from this build.

- Install [.Net Core SDK](https://www.microsoft.com/net/download/linux-package-manager/ubuntu14-04/sdk-current)
- go to the project folder
- ./build.sh -t Mono

**Linux(Dotnet Core only)**:

- this does only build projects targeting netstandard2.0 (Core, BioContainers, IO, Stats, ML)

- Install [.Net Core SDK](https://www.microsoft.com/net/download/linux-package-manager/ubuntu14-04/sdk-current)
- go to the project folder
- ./build.sh -t Dotnet

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
// Returns the corresponding nucleotide of the complementary strand
let antiG = Nucleotides.G |> Nucleotides.complement
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

The library is available under the OSI-approved MIT license. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/CSBiology/BioFSharp/tree/master/docsrc/content
  [gh]: https://github.com/CSBiology/BioFSharp
  [issues]: https://github.com/CSBiology/BioFSharp/issues
  [readme]: https://github.com/CSBiology/BioFSharp/blob/master/README.md
  [license]: https://github.com/CSBiology/BioFSharp/blob/master/LICENSE
*)