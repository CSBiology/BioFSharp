module AminoAcidTests

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

let allParsedAAs = [
    StandardCodes AminoAcid.Ala; StandardCodes AminoAcid.Cys; StandardCodes AminoAcid.Asp
    StandardCodes AminoAcid.Glu; StandardCodes AminoAcid.Phe; StandardCodes AminoAcid.Gly
    StandardCodes AminoAcid.His; StandardCodes AminoAcid.Ile; StandardCodes AminoAcid.Lys
    StandardCodes AminoAcid.Leu; StandardCodes AminoAcid.Met; StandardCodes AminoAcid.Asn
    StandardCodes AminoAcid.Pyl; StandardCodes AminoAcid.Pro; StandardCodes AminoAcid.Gln
    StandardCodes AminoAcid.Arg; StandardCodes AminoAcid.Ser; StandardCodes AminoAcid.Thr
    StandardCodes AminoAcid.Sel; StandardCodes AminoAcid.Sel; StandardCodes AminoAcid.Val
    StandardCodes AminoAcid.Trp; StandardCodes AminoAcid.Tyr; AmbiguityCodes AminoAcid.Xaa
    AmbiguityCodes AminoAcid.Xle; AmbiguityCodes AminoAcid.Glx; AmbiguityCodes AminoAcid.Asx
    GapTer AminoAcid.Gap; GapTer AminoAcid.Ter; NoAAChar '!'
]

let allModFormulas = (allFormulas |> List.map(fun f -> Formula.replaceElement f Elements.Table.N Elements.Table.Heavy.N15))

let allSingleModAAs = allAAs |> List.map (fun aa -> AminoAcids.Mod (aa, [ModificationInfo.Table.N15]))
let allDoubleModAAs = allAAs |> List.map (fun aa -> AminoAcids.Mod (aa, [ModificationInfo.Table.N15; ModificationInfo.Table.H2O]))

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
        testCase "formulaModifiedAA" (fun () ->
            let modFormula = Formula.replaceElement Formula.Table.Ala Elements.Table.N Elements.Table.Heavy.N15
            Expect.equal
                (testModifiedAA |> AminoAcids.formula)
                modFormula
                "AminoAcids.formula did not return the correct formula for a 15N modified Alanine"
        )
        testCase "isTerminator" (fun () ->
            Expect.isTrue
                (AminoAcids.isTerminator AminoAcid.Ter) 
                "AminoAcids.isTerminator did not return true for a Terminator"
        )
        testCase "isGap" (fun () ->
            Expect.isTrue
                (AminoAcids.isGap AminoAcid.Gap)
                "AminoAcids.isGap did not return true for a Gap"
        )
        testCase "setModification" (fun () ->
            Expect.equal
                (allAAs |> List.map (fun aa -> AminoAcids.setModification ModificationInfo.Table.N15 aa))
                allSingleModAAs
                "AminoAcids.setModification did not return the correct Modifications for all AminoAcids"
        )
        testCase "setModifications" (fun () ->
            let modList = [ModificationInfo.Table.N15; ModificationInfo.Table.H2O]
            Expect.equal
                (allAAs |> List.map (fun aa -> AminoAcids.setModifications modList aa))
                allDoubleModAAs
                "AminoAcids.setModifications did not return the correct Modifications for all AminoAcids"
        )
        testCase "getModifications" (fun () ->
            let allMods = [for i in 1 .. 29 -> [ModificationInfo.Table.N15]]
            Expect.equal
                (allSingleModAAs |> List.map (fun aa -> AminoAcids.getModifications aa))
                allMods
                "AminoAcids.getModifications did not return correct Modifications for all modified AminoAcids"
        )

        testCase "getModifications_withUnmodAAs" (fun () ->
            let allEmptyLists = [for i in 1 .. 29 -> []]
            Expect.equal
                (allAAs |> List.map (fun aa -> AminoAcids.getModifications aa))
                allEmptyLists
                "AminoAcids.getModifications did not return correct empty list for all unmodified AminoAcids"
        )
        testCase "getAminoAcidWithoutMod" (fun () ->
            Expect.equal    
                (allSingleModAAs |> List.map (fun aa -> AminoAcids.getAminoAcidWithoutMod aa))
                allAAs
                "AminoAcids.getAminoAcidWithoutMod did not return correct AminoAcid for all AminoAcids"
        )
        testCase "tryGetModifications" (fun () ->
            let allMods = [for i in 1 .. 29 -> Some [ModificationInfo.Table.N15]]
            Expect.equal
                (allSingleModAAs |> List.map (fun aa -> AminoAcids.tryGetModifications aa))
                allMods
                "AminoAcids.tryGetModifications did not return correct Modifications for all modified AminoAcids"
        )
        testCase "tryGetModifications_withNone" (fun () ->
            Expect.equal
                (allAAs |> List.map (fun aa -> AminoAcids.tryGetModifications aa))
                [for i in 1 .. 29 -> None]
                "AminoAcids.tryGetModifications did not return correct Modifications for all unmodified AminoAcids"
        )
        testCase "isotopicLabelFunc" (fun () ->
            Expect.equal
                (List.map2 (fun aa f -> AminoAcids.isotopicLabelFunc aa f) allSingleModAAs allFormulas)
                allModFormulas
                "AminoAcids.isotopicLabelFunc did not return correct function for all Modified AminoAcids"    
        )
        testCase "isotopicLabelFunc_withUnmodifiedAAs" (fun () ->
            Expect.equal
                (List.map2 (fun aa f -> AminoAcids.isotopicLabelFunc aa f) allAAs allFormulas)
                allFormulas
                "AminoAcids.isotopicLabelFunc did not return correct function for all unmodified AminoAcids"
        )
        testCase "charToParsedAminoAcidChar" (fun () -> // This function currently matches to Sel, not Sec. Will eventually need chane
            let testParsedSymbols = List.append allSymbols ['!']
            Expect.equal
                (List.map (fun c -> AminoAcids.charToParsedAminoAcidChar c) testParsedSymbols)
                allParsedAAs
                "AminoAcids.charToParsedAminoAcidChar did not return correct AminoAcid from all AminoAcid chars"
        )
        testCase "aminoAcidSetStandard" (fun () ->
            let testAASetStandard = set [
                AminoAcid.Ala; AminoAcid.Cys; AminoAcid.Asp; AminoAcid.Glu
                AminoAcid.Phe; AminoAcid.Gly; AminoAcid.His; AminoAcid.Ile
                AminoAcid.Lys; AminoAcid.Leu; AminoAcid.Met; AminoAcid.Asn
                AminoAcid.Pro; AminoAcid.Gln; AminoAcid.Arg; AminoAcid.Ser
                AminoAcid.Thr; AminoAcid.Val; AminoAcid.Trp; AminoAcid.Tyr
            ]
            Expect.equal
                AminoAcids.aminoAcidSetStandard
                testAASetStandard
                "AminoAcids.aminoAcidSetStandard did not return correct standard set of AminoAcids"
        )
        testCase "aminoAcidSetProteinogenic" (fun () ->
            let testAASetProteinogenic = set [
                AminoAcid.Ala; AminoAcid.Cys; AminoAcid.Asp; AminoAcid.Glu
                AminoAcid.Phe; AminoAcid.Gly; AminoAcid.His; AminoAcid.Ile
                AminoAcid.Lys; AminoAcid.Leu; AminoAcid.Met; AminoAcid.Asn
                AminoAcid.Pro; AminoAcid.Gln; AminoAcid.Arg; AminoAcid.Ser
                AminoAcid.Thr; AminoAcid.Val; AminoAcid.Trp; AminoAcid.Tyr
                AminoAcid.Sel; AminoAcid.Sec; AminoAcid.Pyl
            ]
            Expect.equal
                AminoAcids.aminoAcidSetProteinogenic
                testAASetProteinogenic
                "AminoAcids.aminoAcidSetProteinogenic did not return correct set of AminoAcids"
        )
        testCase "aminoAcidSetProteinogenicEucaryotes" (fun () ->
            let testAASetProteinogenicEucaryotes = set [
                AminoAcid.Ala; AminoAcid.Cys; AminoAcid.Asp; AminoAcid.Glu
                AminoAcid.Phe; AminoAcid.Gly; AminoAcid.His; AminoAcid.Ile
                AminoAcid.Lys; AminoAcid.Leu; AminoAcid.Met; AminoAcid.Asn
                AminoAcid.Pro; AminoAcid.Gln; AminoAcid.Arg; AminoAcid.Ser
                AminoAcid.Thr; AminoAcid.Val; AminoAcid.Trp; AminoAcid.Tyr
                AminoAcid.Sel; AminoAcid.Sec;
            ]
            Expect.equal
                AminoAcids.aminoAcidSetProteinogenicEucaryotes
                testAASetProteinogenicEucaryotes
                "AminoAcids.aminoAcidSetProteinogenicEucaryotes did not return correct set of AminoAcids"
        )
        testCase "aminoAcidSetAmbiguity" (fun () ->
            let testAASetAmbiguity = set [
                AminoAcid.Xaa; AminoAcid.Xle; AminoAcid.Glx; AminoAcid.Asx
            ]
            Expect.equal
                AminoAcids.aminoAcidSetAmbiguity
                testAASetAmbiguity
                "AminoAcids.aminoAcidSetAmbiguity did not return the correct set of AminoAcids"
        )
        testCase "aminoAcidSetGapTer" (fun () ->
            let testAASetGapTer = set [AminoAcid.Gap; AminoAcid.Ter]
            Expect.equal
                AminoAcids.aminoAcidSetGapTer
                testAASetGapTer
                "AminoAcids.aminoAcidSetGapTer did not return the correct set of AminoAcids"
        )
        testCase "aminoAcidSetPosCharged" (fun () ->
            let testAAPosCharged = set [AminoAcid.Arg; AminoAcid.Lys; AminoAcid.His]
            Expect.equal
                AminoAcids.aminoAcidSetPosCharged
                testAAPosCharged
                "AminoAcids.aminoAcidSetPosCharged did not return the correct set of AminoAcids"
        )
        testCase "aminoAcidSetNegCharged" (fun () ->
            let testAANegCharged = set [
                AminoAcid.Asp; AminoAcid.Glu
            ]
            Expect.equal
                AminoAcids.aminoAcidSetNegCharged
                testAANegCharged
                "AminoAcids.aminoAcidSetNegCharged did not return the correct set of AminoAcids"
        )
        testCase "aminoAcidSetPolarUncharged" (fun () ->
            let testAASetPolarUncharged = set [
                AminoAcid.Gln; AminoAcid.Asn; AminoAcid.Ser; AminoAcid.Thr
            ]
            Expect.equal
                AminoAcids.aminoAcidSetPolarUncharged
                testAASetPolarUncharged
                "AminoAcids.aminoAcidSetPolarUncharged did not return the correct set of AminoAcids"
        )
        testCase "aminoAcidSetHydrophobic" (fun () ->
            let testAASetHydrophobic = set [
                AminoAcid.Ala; AminoAcid.Ile; AminoAcid.Leu; AminoAcid.Met
                AminoAcid.Phe; AminoAcid.Trp; AminoAcid.Tyr; AminoAcid.Val
            ]
            Expect.equal
                AminoAcids.aminoAcidSetHydrophobic
                testAASetHydrophobic
                "AminoAcids.aminoAcidSetHydrophobic did not return the correct set of AminoAcids"
        )
        testCase "aminoAcidSetSpecialCases" (fun () ->
            let testAASetSpecialCases = set [
                AminoAcid.Cys; AminoAcid.Sel; AminoAcid.Sec; AminoAcid.Gly
                AminoAcid.Pro
            ]
            Expect.equal
                AminoAcids.aminoAcidSetSpecialCases
                testAASetSpecialCases
                "AminoAcids.aminoAcidSetSpecialCases did not return the correct set of AminoAcids"
        )
        testCase "isPolarUncharged" (fun () ->
            let testIsPolarUncharged = [
                false; false; false; false
                false; false; false; false
                false; false; false; true
                false; false; true;  false
                true;  true;  false; false; false
                false; false; false; false
                false; false; false; false
            ]
            Expect.equal
                (allAAs |> List.map (fun aa -> AminoAcids.isPolarUncharged aa))
                testIsPolarUncharged
                "AminoAcids.isPolarUncharged did not return correct boolean for each AminoAcid"
        )
        testCase "isModified_withUnmodAAs" (fun () ->
            let testIsModifiedFalse = [for i in 1 .. 29 -> false]
            Expect.equal
                (allAAs |> List.map (fun aa -> AminoAcids.isModified aa))
                testIsModifiedFalse
                "AminoAcids.isModified did not return correct boolean for each unmodified AminoAcid"
        )
        testCase "isModified_withModAAs" (fun () ->
            let testIsModifiedTrue = [for i in 1 .. 29 -> true]
            Expect.equal
                (allSingleModAAs |> List.map (fun aa -> AminoAcids.isModified aa))
                testIsModifiedTrue
                "AminoAcids.isModified did not return correct boolean for each modified AminoAcid"
        )
    ]