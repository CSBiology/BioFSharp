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



open AminoAcidSymbols

let aminoPropertyBy defaultValue (str:string) =
    let arr = 
        str.Split(' ')
        |> Array.map (fun v -> 
            match parseChar v.[0] with
            | _,Some ac -> int ac - 65, float v.[2..] 
            | _ -> failwith "Error in AminoAcid code at given property" )
    
    // TODO: validation

    let av = Array.create 25 defaultValue
    for a in arr do 
        av.[fst a] <- snd a
    
    (fun  (amino:AminoAcidSymbol) -> av.[int amino - 65])
        
let getHydrophobicityIndex = aminoPropertyBy nan "A:0.61 R:0.60 N:0.06 D:0.46 C:1.07 Q:0. E:0.47 G:0.07 H:0.61 I:2.22 L:1.53 K:1.15 M:1.18 F:2.02 P:1.95 S:0.05 T:0.05 W:2.65 Y:1.88 V:1.32"

getHydrophobicityIndex AminoAcidSymbol.Asn


int AminoAcidSymbol.Asn
26 + 65 |> aminoAcidSymbol
