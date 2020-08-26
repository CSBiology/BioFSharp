module NucleotideTests

open BioFSharp
open Nucleotides
open Formula
open Expecto

let allNucs = [
    Nucleotide.A; Nucleotide.T; Nucleotide.G; Nucleotide.C; Nucleotide.U
    Nucleotide.I; Nucleotide.Gap; Nucleotide.Ter; Nucleotide.R
    Nucleotide.Y; Nucleotide.K; Nucleotide.M; Nucleotide.S
    Nucleotide.W; Nucleotide.B; Nucleotide.D; Nucleotide.H
    Nucleotide.V; Nucleotide.N
]
let allSymbols = ['A';'T';'G';'C';'U';'I';'-';'*';'R';'Y';'K';'M';'S';'W';'B';'D';'H';'V';'N']
let allFormuals = [
    Formula.Table.A; Formula.Table.T; Formula.Table.G; Formula.Table.C; Formula.Table.U
    Formula.Table.I; Formula.emptyFormula; Formula.emptyFormula; Formula.emptyFormula
    Formula.emptyFormula; Formula.emptyFormula; Formula.emptyFormula; Formula.emptyFormula
    Formula.emptyFormula; Formula.emptyFormula; Formula.emptyFormula; Formula.emptyFormula
    Formula.emptyFormula; Formula.emptyFormula
]
let allNames = [
    "Adenine"; "Thymidine"; "Guanine"; "Cytosine"; "Uracil"
    "Inosine"; "Gap"      ; "Ter"    ; "puRine"  ; "pYrimidine"
    "Keto"   ; "aMino"    ; "Strong base pair"   ; "Weak base pair"
    "not A"  ; "not C"    ; "not G"  ; "not T/U" ;  "Unspecified"
]
let allParsedNucChars = [
    StandardCodes Nucleotide.A; Standard_DNAonly Nucleotide.T; StandardCodes Nucleotide.G
    StandardCodes Nucleotide.C; Standard_RNAonly Nucleotide.U; Standard_RNAonly Nucleotide.I
    GapTer Nucleotide.Gap; GapTer Nucleotide.Ter; AmbiguityCodes Nucleotide.R
    AmbiguityCodes Nucleotide.Y; AmbiguityCodes Nucleotide.K; AmbiguityCodes Nucleotide.M
    AmbiguityCodes Nucleotide.S; AmbiguityCodes Nucleotide.W; AmbiguityCodes Nucleotide.B
    AmbiguityCodes Nucleotide.D; AmbiguityCodes Nucleotide.H; AmbiguityCodes Nucleotide.V
    AmbiguityCodes Nucleotide.N; NoNucChar '!'
]

[<Tests>]
let nucleotideTests =
    testList "Nucleotides" [
        testCase "symbol" (fun() ->
            let testSymbols = allNucs |> List.map(fun nuc -> Nucleotides.symbol nuc)
            Expect.equal
                testSymbols
                allSymbols
                "Nucleotides.symbol did not return the correct symbol for all Nucleotides."
        )
        testCase "formula" (fun() ->
            let testFormulas = allNucs |> List.map(fun nuc -> Nucleotides.formula nuc)
            Expect.equal
                testFormulas
                allFormuals
                "Nucleotides.formula did not return the correct formula for all Nucleotides."
        )
        testCase "isTerminator" (fun() ->
            let testBools = [
                false; false; false; false; false; false; false; true; false
                false; false; false; false; false; false; false; false; false
                false
            ]
            Expect.equal
                (allNucs |> List.map(fun nuc -> Nucleotides.isTerminator nuc))
                testBools
                "Nucleotides.isTerminator did not return the correct boolean for all Nucleotides."
        )
        testCase "isGap" (fun() ->
            let testBools = [
                false; false; false; false; false; false; true; false; false
                false; false; false; false; false; false; false; false; false
                false
            ]
            Expect.equal
                (allNucs |> List.map(fun nuc -> Nucleotides.isGap nuc))
                testBools
                "Nucleotides.isGap did not return the correct boolean for all Nucleotides."
        )
        testCase "name" (fun() ->
            let testNames = (allNucs |> List.map(fun nuc -> Nucleotides.name nuc))
            Expect.equal
                testNames
                allNames
                "Nucleotides.name did not return the correct name for all nucleotides."
        )
        testCase "charToParsedNucleotideChar" (fun() ->
            let testSymbols = List.append allSymbols ['!']
            let testParsedNucChars = (testSymbols |> List.map(fun nuc -> Nucleotides.charToParsedNucleotideChar nuc))
            Expect.equal
                testParsedNucChars
                allParsedNucChars
                "Nucleotides.charToParsedNucleotideChar did not return correct ParsedNucleotideChar for all Nucleotides."
        )
        testCase "complement" (fun () ->
            let allComplementNucs = [
                Nucleotide.T; Nucleotide.A; Nucleotide.C; Nucleotide.G; Nucleotide.A
                Nucleotide.I; Nucleotide.Gap; Nucleotide.Ter; Nucleotide.Y; Nucleotide.R
                Nucleotide.M; Nucleotide.K; Nucleotide.S; Nucleotide.W; Nucleotide.V
                Nucleotide.H; Nucleotide.D; Nucleotide.B; Nucleotide.N
            ]
            let testComplementNucs = (allNucs |> List.map(fun nuc -> Nucleotides.complement nuc))
            Expect.equal
                testComplementNucs
                allComplementNucs
                "Nucleotides.complement did not return the correct complement Nucleotide for all Nucleotides."
        )
        testCase "replaceTbyU" (fun () ->
            let allNucsReplaceTbyU = [
                Nucleotide.A; Nucleotide.U; Nucleotide.G; Nucleotide.C; Nucleotide.U
                Nucleotide.I; Nucleotide.Gap; Nucleotide.Ter; Nucleotide.R
                Nucleotide.Y; Nucleotide.K; Nucleotide.M; Nucleotide.S; Nucleotide.W
                Nucleotide.B; Nucleotide.D; Nucleotide.H; Nucleotide.V; Nucleotide.N
            ]
            let testNucsReplaceTbyU = (allNucs |> List.map(fun nuc -> Nucleotides.replaceTbyU nuc))
            Expect.equal
                testNucsReplaceTbyU
                allNucsReplaceTbyU
                "Nucleotides.replaceTbyU did not return the correct Nucleotide for all Nucleotides."
        )
        testCase "replaceUbyT" (fun () ->
            let allNucsReplaceUbyT = [
                Nucleotide.A; Nucleotide.T; Nucleotide.G; Nucleotide.C; Nucleotide.T
                Nucleotide.I; Nucleotide.Gap; Nucleotide.Ter; Nucleotide.R
                Nucleotide.Y; Nucleotide.K; Nucleotide.M; Nucleotide.S; Nucleotide.W
                Nucleotide.B; Nucleotide.D; Nucleotide.H; Nucleotide.V; Nucleotide.N
            ]
            let testNucsReplaceUbyT = (allNucs |> List.map(fun nuc -> Nucleotides.replaceUbyT nuc))
            Expect.equal
                testNucsReplaceUbyT
                allNucsReplaceUbyT
                "Nucleotides.replaceUbyT did not return the correct Nucleotide for all Nucleotides."
        )
    ]
