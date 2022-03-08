(**
---
title: Formulas
category: BioFSharp Core
categoryindex: 1
index: 1
---
*)

(*** hide ***)

(*** condition: prepare ***)
#r "nuget: FSharpAux, 1.1.0"
#r "nuget: FSharpAux.IO, 1.1.0"
#r "nuget: FSharp.Stats, 0.4.3"
#r "nuget: Plotly.NET, 2.0.0-preview.18"
#r "../src/BioFSharp/bin/Release/netstandard2.0/BioFSharp.dll"
#r "../src/BioFSharp.IO/bin/Release/netstandard2.0/BioFSharp.IO.dll"
#r "../src/BioFSharp.BioContainers/bin/Release/netstandard2.0/BioFSharp.BioContainers.dll"
#r "../src/BioFSharp.ML/bin/Release/netstandard2.0/BioFSharp.ML.dll"
#r "../src/BioFSharp.Stats/bin/Release/netstandard2.0/BioFSharp.Stats.dll"

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
# Formula

[![Binder]({{root}}img/badge-binder.svg)](https://mybinder.org/v2/gh/CSBiology/BioFSharp/gh-pages?filepath={{fsdocs-source-basename}}.ipynb)&emsp;
[![Script]({{root}}img/badge-script.svg)]({{root}}{{fsdocs-source-basename}}.fsx)&emsp;
[![Notebook]({{root}}img/badge-notebook.svg)]({{root}}{{fsdocs-source-basename}}.ipynb)

*Summary:* This example shows how to use chemical formulas in BioFSharp

BioFSharp offers a great bunch of functionality for working with molecules. All elements are represented as the composition of their stable isotopes. A `Formula` is a collection of those Elements with the given count. Creating and altering formulas is quite easy. Also functions for obtaining a mass of a molecule, which becomes quite handy especially for mass spectrometry, can be used straightforwardly.  

To create formulas, no direct fiddling around with the data type is necessary. You can just use the stringparser:
*)
open BioFSharp


let CO2 = Formula.parseFormulaString "CO2"
Formula.toString CO2 // val it : string = "C1.00 O2.00 "

(**
We just created some Carbon Dioxide. Luckily there is no in silico climate change. But let's get rid of it anyways, by making some <a data-toggle="collapse" data-target="#sprudel">Sprudel\*</a>:<div id="sprudel" class="collapse Sprudel">_\*german term for sprinkly water_</div>
*)
let sprudel = Formula.add CO2 (Formula.Table.H2O)
Formula.toString sprudel // val it : string = "C1.00 H2.00 O3.00 "

(**
Quite refreshing, but boring nevertheless. Let's make some radioactive sprudel.
*)

/// create a monoisotopic carbon consisting only of C14
let monoC14 = 
    Elements.createMono "C14" (Isotopes.Table.C14,1.)
    |> Elements.Mono 

/// exchanges all carbon in formula with monoIsotopic C14
let lableWithC14 molecule = Formula.replaceElement molecule Elements.Table.C monoC14

let radioactiveSprudel = lableWithC14 sprudel


(**
As you can see converting a refreshing drink to a refreshing, radioactive drink is quickly done. As a check up, let's compare the masses:
*)

Formula.monoisoMass sprudel // val it : float = 62.00039392
Formula.monoisoMass radioactiveSprudel // val it : float = 64.00363591
