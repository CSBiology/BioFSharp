(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "../../packages/build/FSharp.Plotly/lib/net40/Fsharp.Plotly.dll"
open FSharp.Plotly
(**
BioFSharp
======================


*)
#r "BioFSharp.dll"
open BioFSharp


let CO2 = Formula.parseFormulaString "CO2"
Formula.toString CO2

let c = Formula.add CO2 CO2
Formula.toString c


AminoAcidSymbols.AminoAcidSymbol.Ala |> int
(**

Converting a peptide string to a biosequence

*)

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





