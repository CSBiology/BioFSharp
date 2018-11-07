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
#r "BioFSharp.dll"
#r "BioFSharp.IO.dll"

(**
<table class="HeadAPI">
<td class="Head"><h1>Sequence properties</h1></td>
<td class="API">
    <a id="APILink" href="https://csbiology.github.io/BioFSharp/reference/biofsharp-aminoproperties.html" >&#128194;View module documentation</a>
</td>
</table>
## General
BioFSharp comes equipped with a range of numerical values for important amino acid properties. To access them in an easy fashion, you can use the `initGetAminoProperty` function in the following way. The result is a mapping function, which assigns a value to each compatible amino acid.  
In this tutorial our aim is to find out the hydrophobicity of a peptide. We start by calling the aforementioned function.
*)

open BioFSharp
open AminoProperties

let getHydrophobicityIndex  = initGetAminoProperty AminoProperty.HydrophobicityIndex

getHydrophobicityIndex AminoAcidSymbols.AminoAcidSymbol.Ala 

let getHydrophobicityIndexZ  = initGetAminoPropertyZnorm AminoProperty.HydrophobicityIndex

getHydrophobicityIndexZ AminoAcidSymbols.AminoAcidSymbol.Ala 

(**
With this function you might easily estimate the hydrophobictiy of our peptide by calling it on every element with a map. Usually close amino acids in a peptide influence each other. To cover this you can use the `ofWindowedBioArray` function. It also takes a window size and calculates the value of the property of every amino acid in the chain with regards to the effect of the adjacent amino acids in this window.
*)
let peptide = 
    "REYAHMIGMEYDTVQK"
    |> BioArray.ofAminoAcidString
    |> Array.map AminoAcidSymbols.aminoAcidSymbol


let peptidehydrophobicites = peptide |> Array.map getHydrophobicityIndex
(**<pre>
val peptidehydrophobicites : float [] =
  [|0.6; 0.47; 1.88; 0.61; 0.61; 1.18; 2.22; 0.07; 1.18; 0.47; 1.88; 0.46;
    0.05; 1.32; 0.0; 1.15|]
</pre>*)   
let peptidehydrophobicites' = peptide |> AminoProperties.ofWindowedBioArray 3 getHydrophobicityIndex
(**<pre>
val peptidehydrophobicites' : float [] =
  [|0.89; 0.834; 0.8916666667; 1.081428571; 1.005714286; 1.107142857;
    0.9057142857; 1.087142857; 1.065714286; 0.9042857143; 0.7757142857;
    0.7657142857; 0.7614285714; 0.81; 0.596; 0.63|]
</pre>  
In the last step you can then just sum or average over the values to get a summary value of the hydrophobicity, depending on wether you want a length dependent or independent value.
*)   

Array.sum peptidehydrophobicites // val it : float = 14.15
Array.sum peptidehydrophobicites' // val it : float = 14.11166667

Array.average peptidehydrophobicites // val it : float = 0.884375
Array.average peptidehydrophobicites' // val it : float = 0.8819791667

(**
##Isoelectric Point

The isoelectric point (pI) of a protein is the point at which it carries as many positive as negative charges. 
Therefore the overall charge is zero. Knowing this value can e.g. be useful for isolation of single proteins in a voltage gradient.  
The implementation is based on: "[http://fields.scripps.edu/DTASelect/20010710-pI-Algorithm.pdf](http://fields.scripps.edu/DTASelect/20010710-pI-Algorithm.pdf)". 
In principle, the distinct amino acids in the protein are counted. 
By using the [Henderson-Hasselbalch equation](https://en.wikipedia.org/wiki/Henderson-Hasselbalch_equation) and the pKr values, the theoretic charge states of the amino acids for a specific pH can be calculated. 
Multiplying those charge states with the count of the associated amino acids and adding those products together then gives the overall charge of the protein. This is only done with the amino acids, which might be charged (basic, acidic). 
The isoelectric point is the pH value for which this function returns zero. It is found by [bisection](https://en.wikipedia.org/wiki/Bisection_method) (also called Binary Search).  
Disclaimer: Keep in mind, that this algorithm ignores post-translational modifications and interactions of the amino acids with each other. Therefore it is only intented to be a rough approximation and should be used as such.
<br>
The function for finding the isoelectric point is found in the `IsoelectricPoint` module. 

* Besides the peptide sequence in form of a `AminoAcidSymbol` Seq, it takes 
* a mapping function, which maps an `AminoAcidSymbol` to a float representing the pKr and
* an accuracy value. The found pI has to be at least as close to zero as this accuracy value

*)

//AA sequence
let myProteinForPI = 
    "ATPIIEMNYPWTMNIKLSSDACMTNWWPNCMTLKIIA"
    |> Seq.map AminoAcidSymbols.aminoAcidSymbol

//default function for pKr of charged aminoacids
let pKrFunction = IsoelectricPoint.getpKr

//accuracy in z
let acc = 0.5

let pI = IsoelectricPoint.tryFind pKrFunction acc myProteinForPI

(**
The result will be of type `Option<float*float>`. The first float is the pH, the second one is the according charge.
*)