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
// 
//
//TODO: Formula.lableElement solle input vom typ formula an letzter stelle haben
//
//

(**
<table class="HeadAPI">
<td class="Head"><h1>Formula</h1></td>
<td class="API">
    <a id="APILink" href="https://csbiology.github.io/BioFSharp/reference/biofsharp-formula.html" >&#128194;View module documentation</a>
</td>
</table>
BioFSharp offers a great bunch of functionality for working with molecules. All elements are represented as the composition of their stable isotopes. A `Formula` is a collection of those Elements with the given count. Creating and altering formulas is quite easy. Also functions for obtaining a mass of a molecule, which becomes quite handy especially for mass spectrometry, can be used straightforwardly.  

To create formulas, no direct fiddling around with the data type is necessary. You can just use the stringparser:
*)
#r "BioFSharp.dll"
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
let lableWithC14 molecule = Formula.lableElement molecule Elements.Table.C monoC14

let radioactiveSprudel = lableWithC14 sprudel


(**
As you can see converting a refreshing drink to a refreshing, radioactive drink is quickly done. As a check up, let's compare the masses:
*)

Formula.monoisoMass sprudel // val it : float = 62.00039392
Formula.monoisoMass radioactiveSprudel // val it : float = 64.00363591
