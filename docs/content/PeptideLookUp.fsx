(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
//#I "../../bin"

(**
BioFSharp
======================


*)
#r "../../bin/BioFSharp.dll"
#r "../../bin/BioFSharp.Mz.dll"

open BioFSharp
open BioFSharp.Mz

#time


let paramTest = 
    SearchDB.createSearchDbParams 
        "Creinhardtii_Cp"
        // path of db storage folder
        (__SOURCE_DIRECTORY__ + "/data/")
        (__SOURCE_DIRECTORY__ + "/data/Chlamy_Cp.fastA")
        Digestion.trypsin
        3
        15000.
        // valid symbol name of isotopic label in label table i.e. #N15
        ""
        SearchDB.MassMode.Monoisotopic
        []
        []





let peptideLookUp = 
    let massF = BioFSharp.BioItem.monoisoMass
    SearchDB.getPeptideLookUpBy paramTest id 5 massF

