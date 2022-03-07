(**
---
title: BioItems
category: BioFSharp Core
categoryindex: 1
index: 2
---
*)

(*** hide ***)

(*** condition: prepare ***)
#r "nuget: FSharpAux, 1.1.0"
#r "nuget: FSharpAux.IO, 1.1.0"
#r "nuget: FSharp.Stats, 0.4.3"
#r "nuget: Plotly.NET, 2.0.0-preview.18"
#r "../bin/BioFSharp/netstandard2.0/BioFSharp.dll"
#r "../bin/BioFSharp.IO/netstandard2.0/BioFSharp.IO.dll"
#r "../bin/BioFSharp.BioContainers/netstandard2.0/BioFSharp.BioContainers.dll"
#r "../bin/BioFSharp.ML/netstandard2.0/BioFSharp.ML.dll"
#r "../bin/BioFSharp.Stats/netstandard2.0/BioFSharp.Stats.dll"

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: FSharpAux, 1.1.0"
#r "nuget: FSharpAux.IO, 1.1.0"
#r "nuget: FSharp.Stats, 0.4.3"
#r "nuget: Plotly.NET, 2.0.0-preview.18"
#r "nuget: Plotly.NET.Interactive, 2.0.0-preview.18"
#r "nuget: BioFSharp, {{fsdocs-package-version}}"
#r "nuget: BioFSharp.IO, {{fsdocs-package-version}}"
#r "nuget: BioFSharp.BioContainers, {{fsdocs-package-version}}"
#r "nuget: BioFSharp.ML, {{fsdocs-package-version}}"
#r "nuget: BioFSharp.Stats, {{fsdocs-package-version}}"
#endif // IPYNB

(**
# BioItems

[![Binder]({{root}}img/badge-binder.svg)](https://mybinder.org/v2/gh/plotly/Plotly.NET/gh-pages?filepath={{fsdocs-source-basename}}.ipynb)&emsp;
[![Script]({{root}}img/badge-script.svg)]({{root}}{{fsdocs-source-basename}}.fsx)&emsp;
[![Notebook]({{root}}img/badge-notebook.svg)]({{root}}{{fsdocs-source-basename}}.ipynb)

*Summary:* This example shows how to use chemical formulas in BioFSharp

Often, dealing with similar problems separately results in different approaches. In a programming background, this might make things needlessly complex. Therefore in BioFSharp nucleotides and amino acids are based on the same structural scaffold, leading to a consistent way of working with them. This can come in handy especially when working with their formulas.  

### Table of contents
- [Basics](#Basics)
- [AminoAcids](#Amino-Acids)
    - [AminoAcids](#Modifying-Amino-Acids)
- [Nucleotides](#Nucleotides)

## Basics

Many functions are similar for AminoAcids and Nucleotides, like for example:
*)

open BioFSharp
open BioFSharp.AminoAcids
open BioFSharp.Nucleotides

(***hide***)

(**Accessing the full name:*)

AminoAcids.name Ala 
(*** include-it ***)

Nucleotides.name G 
(*** include-it ***)

(**or the underlying chemical formula:*)

AminoAcids.formula Lys 
|> Formula.toString 
(*** include-it ***)

Nucleotides.formula T 
|> Formula.toString 
(*** include-it ***)

(**
Nucleotides and AminoAcids in BioFSharp are represented as Union cases. This makes applying functions selectively very easy. 
*)
let filterLysine aa = 
    match aa with
    | AminoAcids.Lys -> AminoAcids.Gap
    | _ -> aa

filterLysine Ala 
(*** include-it ***)

filterLysine Lys
(*** include-it ***)

(**
Of course some functions like these are already defined. Let's use a predefined function to find charged amino acids.

*)

let giveMePositiveAAs aminoAcid = 
    match aminoAcid with
    | a when AminoAcids.isPosCharged a -> 
        printfn 
            "Hey, how are you? I am %s, but my friends call me %c. I'm usually in a positive mood"
            (AminoAcids.name a)
            (AminoAcids.symbol a)

    | a when AminoAcids.isNegCharged a -> 
        printfn 
            "I am %s, short: %c. I'm usually in a negative mood"
            (AminoAcids.name a)
            (AminoAcids.symbol a)

    | _ -> printfn "Just strolling around, minding my own business."

(**Alanine is usually not charged:*)

giveMePositiveAAs Ala
(*** include-output ***)

(**Lysine is usually positively charged:*)

giveMePositiveAAs Lys
(*** include-output ***)

(**Glutamic acid is usually negatively charged:*)

giveMePositiveAAs Glu
(*** include-output ***)


(**

## Amino Acids

### Modifying Amino Acids

What makes working on Amino Acids with BioFSharp truly powerful is the ability to easily modify AminoAcids, even altering their mass and formula. In the following example we try to find out the mass of a phosphorylated Serine. Applications like these might be quite usefull for identification of peptides in mass spectrometry. 
*)

Ser
|> AminoAcids.formula 
|> Formula.toString
(***include-it***)

(**
As you can see by the formula. Our Serine is missing two H and an O. In BioFSharp, all Amino Acids are dehydrolysed by default, because it is assumed that the user will use collections representing a peptide, rather than single Amino Acids. For our cause we want serine in hydrolysed form. An easy way to achieve this is to modify it. An addition of H2O is quite common and therefore premade: 
*)

///Hydrolysed serine

let hydroSerine = AminoAcids.setModification ModificationInfo.Table.H2O Ser

hydroSerine
|> AminoAcids.formula 
|> Formula.toString
(***include-it***)

(**
So far so good. Now let's add the phosphate. For this we first create a function which alters the formula of a given molecule in the way a phosphorylation would. In the second step we create a modification resembling a phosphorylation of a residual. At last we modify our Serine with this modification.
*)

///Phosphorylation of OH-Groups adds PO3 to formula and removes one H
let phosporylate formula =  
    Formula.add (Formula.parseFormulaString "PO3") formula
    |> Formula.substract (Formula.parseFormulaString "H")

//We create a modification at the residual called phosphorylation which in our case is hypothetical, hence the `false` for the 'isBiological` parameter
let phosphorylation = ModificationInfo.createModification "Phosphorylation" false ModificationInfo.ModLocation.Residual phosporylate

///phosphorylated Serine
let phosphoSerine = AminoAcids.setModification phosphorylation hydroSerine

phosphoSerine 
|> AminoAcids.formula 
|> Formula.toString
(***include-it***)

(**
As you can see the Serine is phosphorylated just as we wanted. Our inital aim was to check the mass, this can be done quite easily:
*)

AminoAcids.averageMass Ser
(***include-it***)

AminoAcids.averageMass phosphoSerine
(***include-it***)

(**
## Nucleotides

As working with nucleotides is usually focused on the sequence of the bases, rather than how they actually look like, the list of nucleotide specific functions would be quite short. Here are some of the basic helper functions: 
*)

let myAdenine = Nucleotides.A 
let myThymine = Nucleotides.complement myAdenine 
(***include-value:myThymine***)

Nucleotides.replaceTbyU myAdenine
(***include-it***)

Nucleotides.replaceTbyU myThymine 
(***include-it***)
