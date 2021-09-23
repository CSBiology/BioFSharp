(*** hide ***)

(*** condition: prepare ***)
#r "nuget: Plotly.NET, 2.0.0-preview.8"
#r "nuget: FSharpAux, 1.0.0"
#r "nuget: FSharpAux.IO, 1.0.0"
#r "nuget: FSharp.Stats, 0.4.0"
#r "../bin/BioFSharp/netstandard2.0/BioFSharp.dll"
#r "../bin/BioFSharp.IO/netstandard2.0/BioFSharp.IO.dll"
#r "../bin/BioFSharp.BioContainers/netstandard2.0/BioFSharp.BioContainers.dll"
#r "../bin/BioFSharp.ML/netstandard2.0/BioFSharp.ML.dll"
#r "../bin/BioFSharp.Stats/netstandard2.0/BioFSharp.Stats.dll"

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Plotly.NET, 2.0.0-beta6"
#r "nuget: FSharpAux, 1.0.0"
#r "nuget: FSharpAux.IO, 1.0.0"
#r "nuget: FSharp.Stats, 0.4.0"
#r "nuget: Plotly.NET.Interactive, 2.0.0-beta6"
#r "nuget: BioFSharp, {{fsdocs-package-version}}"
#r "nuget: BioFSharp.IO, {{fsdocs-package-version}}"
#r "nuget: BioFSharp.BioContainers, {{fsdocs-package-version}}"
#r "nuget: BioFSharp.ML, {{fsdocs-package-version}}"
#r "nuget: BioFSharp.Stats, {{fsdocs-package-version}}"
#endif // IPYNB

(**
# BioFSharp

[![Binder]({{root}}img/badge-binder.svg)](https://mybinder.org/v2/gh/plotly/Plotly.NET/gh-pages?filepath={{fsdocs-source-basename}}.ipynb)&emsp;
[![Script]({{root}}img/badge-script.svg)]({{root}}{{fsdocs-source-basename}}.fsx)&emsp;
[![Notebook]({{root}}img/badge-notebook.svg)]({{root}}{{fsdocs-source-basename}}.ipynb)

BioFSharp aims to be a user-friendly functional library for bioinformatics written in F#. It contains the basic data structures for common biological objects like amino acids and nucleotides based on chemical formulas and chemical elements.

BioFSharp facilitates working with sequences in a strongly typed way and is designed to work well with F# Interactive.
It provides a variety of parsers for many biological file formats and a variety of algorithms suited for bioinformatic workflows.

The core datamodel implements in ascending hierarchical order:

- Chemical elements and [formulas](https://csbiology.github.io/BioFSharp/Formula.html) which are a collection of elements
- Amino Acids, Nucleotides and Modifications, which all implement the common [IBioItem interface](https://csbiology.github.io/BioFSharp/BioItem.html#Basics)
- [BioCollections](https://csbiology.github.io/BioFSharp/BioCollections.html) (BioItem,BioList,BioSeq) as representation of biological sequences

</br>

![Data model](https://i.imgur.com/LXBvhmi.png)

</br>

---

## Installation

### For applications and libraries

You can find all available package versions on [nuget](https://www.nuget.org/packages?q=BioFSharp).

 - dotnet CLI

    ```shell
    dotnet add package BioFSharp --version {{fsdocs-package-version}}
    ```

 - paket CLI

    ```shell
    paket add BioFSharp --version {{fsdocs-package-version}}
    ```

 - package manager

    ```shell
    Install-Package BioFSharp -Version {{fsdocs-package-version}}
    ```

    Or add the package reference directly to your `.*proj` file:

    ```
    <PackageReference Include="BioFSharp" Version="{{fsdocs-package-version}}" />
    ```

### For scripting and interactive notebooks
You can include the package via an inline package reference:

```
#r "nuget: BioFSharp, {{fsdocs-package-version}}"
```

---

## Example

The following example shows how easy it is to start working with sequences:
*)

open BioFSharp

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

// Returns the monoisotopic mass of Arginine (minus H2O)
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
BioFSharp.IO also provides pretty printers for BioSequences:
*)

fsi.AddPrinter(BioFSharp.IO.FSIPrinters.prettyPrintBioCollection)

firstItem.Sequence

(***hide***)
let pretty = firstItem.Sequence |> BioFSharp.IO.FSIPrinters.prettyPrintBioCollection

(***include-value: pretty***)

(**
For more detailed examples continue to explore the BioFSharp documentation.
In the near future we will start to provide a cookbook like tutorial in the [CSBlog](https://csbiology.github.io/CSBlog/).

## Contributing and copyright

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][docs] that can be turned into a documentation.

The library is available under the OSI-approved MIT license. For more information see the 
[License file][license] in the GitHub repository. 

  [docs]: https://github.com/CSBiology/BioFSharp/tree/master/docs
  [gh]: https://github.com/CSBiology/BioFSharp
  [issues]: https://github.com/CSBiology/BioFSharp/issues
  [license]: https://github.com/CSBiology/BioFSharp/blob/master/LICENSE
*)
