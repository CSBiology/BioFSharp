(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "../../packages/build/FSharp.Plotly/lib/net40/Fsharp.Plotly.dll"
open FSharp.Plotly
(**
BioSeq
======
work in progress... 

BioList
=======
work in progress... 

BioArray
========
work in progress... 

*)



















(*** hide ***)
#r "BioFSharp.dll"
open BioFSharp


let CO2 = Formula.parseFormulaString "CO2"
Formula.toString CO2

let c = Formula.add CO2 CO2
Formula.toString c


AminoAcidSymbols.AminoAcidSymbol.Ala |> int


//Converting a peptide string to a biosequence



let peptide1 = 
    "REYAHMIGMEYDTVQK"
    |> BioSeq.ofAminoAcidString

let peptide2 = 
    "REYAHMIGMEYDTVQK"
    |> BioSeq.ofAminoAcidString



let fAlanin = 
    Formula.parseFormulaString "C3H5ON" 

fAlanin |> Formula.monoisoMass

fAlanin |> Formula.averageMass

Formula.add fAlanin Formula.Table.H2O  |> Formula.monoisoMass



let carboxyAmidoMethylation =
    ModificationInfo.createModificationWithAdd "CarboxyAmidoMethylation"
                                                ModificationInfo.ModLocation.Residual
                                                "CH3"
 
//Carboxyamidomethylated Cysteine

AminoAcids.Cys
|> AminoAcids.setModification carboxyAmidoMethylation
|> AminoAcids.formula


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
