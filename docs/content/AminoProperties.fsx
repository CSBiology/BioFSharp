(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "../../packages/build/FSharp.Plotly/lib/net45/Fsharp.Plotly.dll"
#r "BioFSharp.dll"
#r "BioFSharp.IO.dll"
(**
#Sequence Properties
*)
open BioFSharp
open AminoProperties

let getHydrophobicityIndex  = initGetAminoProperty AminoProperty.HydrophobicityIndex

getHydrophobicityIndex AminoAcidSymbols.AminoAcidSymbol.Ala 

let getHydrophobicityIndexZ  = initGetAminoPropertyZnorm AminoProperty.HydrophobicityIndex

getHydrophobicityIndexZ AminoAcidSymbols.AminoAcidSymbol.Ala 


let peptide2' = 
    "REYAHMIGMEYDTVQK"
    |> BioArray .ofAminoAcidString
    |> Array.map AminoAcidSymbols.aminoAcidSymbol


peptide2' |> Array.map getHydrophobicityIndex
peptide2' |> AminoProperties.ofWindowedBioArray 3 getHydrophobicityIndex


  
BioItem.formula AminoAcidSymbols.AminoAcidSymbol.Ala

Formula.Table.Ala 

AminoAcidSymbols.name AminoAcidSymbols.AminoAcidSymbol.Ala

(**
##Isoelectric Point
coming soon
*)