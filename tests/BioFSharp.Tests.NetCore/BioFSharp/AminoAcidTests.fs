﻿module AminoAcidTests

open BioFSharp
open AminoAcids
open Expecto

let allSymbols = ['A';'C';'D';'E';'F';'G';'H';'I';'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';'U';'U';'V';'W';'Y';'X';'J';'Z';'B';'-';'*']

let allNames = [
    "Alanine"                 ; "Cysteine"                 ; "Aspartic Acid"  ; "Glutamic Acid"     
    "Phenylalanine"           ; "Glycine"                  ; "Histidine"      ; "Isoleucine"        
    "Lysine"                  ; "Leucine"                  ; "Methionine"     ; "Asparagine"        
    "Pyrrolysine"             ; "Proline"                  ; "Glutamine"      ; "Arginine"          
    "Serine"                  ; "Threonine"                ; "Selenocysteine" ; "Selenocysteine" ; "Valine"            
    "Tryptophan"              ; "Tyrosine"                 ; "Unspecified"    ; "Leucine/Isoleucine"
    "Glutamine/glutamic acid" ; "Asparagine/aspartic acid" ; "Gap"            ; "Ter"               
]

let allFormulas = [
    Formula.Table.Ala ; Formula.Table.Cys ; Formula.Table.Asp    ; Formula.Table.Glu
    Formula.Table.Phe ; Formula.Table.Gly ; Formula.Table.His    ; Formula.Table.Ile
    Formula.Table.Lys ; Formula.Table.Leu ; Formula.Table.Met    ; Formula.Table.Asn
    Formula.Table.Pyl ; Formula.Table.Pro ; Formula.Table.Gln    ; Formula.Table.Arg
    Formula.Table.Ser ; Formula.Table.Thr ; Formula.Table.Sel    ; Formula.Table.Sec; Formula.Table.Val
    Formula.Table.Trp ; Formula.Table.Tyr ; Formula.Table.Xaa    ; Formula.Table.Xle
    Formula.Table.Glx ; Formula.Table.Asx ; Formula.emptyFormula ; Formula.emptyFormula
]

let testModifiedAA = AminoAcid.Mod (AminoAcid.Ala,[ModificationInfo.Table.N15])

let allAAs = [
    AminoAcid.Ala; AminoAcid.Cys; AminoAcid.Asp; AminoAcid.Glu
    AminoAcid.Phe; AminoAcid.Gly; AminoAcid.His; AminoAcid.Ile
    AminoAcid.Lys; AminoAcid.Leu; AminoAcid.Met; AminoAcid.Asn
    AminoAcid.Pyl; AminoAcid.Pro; AminoAcid.Gln; AminoAcid.Arg
    AminoAcid.Ser; AminoAcid.Thr; AminoAcid.Sel; AminoAcid.Sec; AminoAcid.Val
    AminoAcid.Trp; AminoAcid.Tyr; AminoAcid.Xaa; AminoAcid.Xle
    AminoAcid.Glx; AminoAcid.Asx; AminoAcid.Gap; AminoAcid.Ter
]

[<Tests>]
let aminoAcidTests = 
    testList "AminoAcids" [
        testCase "symbol" (fun () -> 
            let testSymbols = allAAs |> List.map (fun aa -> AminoAcids.symbol aa)
            Expect.sequenceEqual
                testSymbols
                allSymbols
                "AminoAcids.symbol did not return the correct symbols for all AminoAcids"
        )
        testCase "symbolModifiedAA" (fun () -> 
            Expect.equal 
                (testModifiedAA |> AminoAcids.symbol)
                'a'
                "AminoAcids.symbol did not return the correct symbols for a 15N modified Alanine"
        )
        testCase "name" (fun () -> 
            let testNames = allAAs |> List.map (fun aa -> AminoAcids.name aa)
            Expect.sequenceEqual
                testNames
                allNames
                "AminoAcids.name did not return the correct name for all AminoAcids"
        )
        testCase "nameModifiedAA" (fun () -> 
           Expect.equal 
               (testModifiedAA |> AminoAcids.name)
               "Alanine[#N15]"
               "AminoAcids.symbol did not return the correct symbols for a 15N modified Alanine"
        )
        testCase "formula" (fun () ->
            let testFormulas = allAAs |> List.map (fun aa -> AminoAcids.formula aa)
            Expect.equal
                testFormulas
                allFormulas
                "AminoAcids.formula did not return the correct formula for all AminoAcids"
        )
    ]