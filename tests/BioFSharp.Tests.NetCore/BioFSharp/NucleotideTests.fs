module NucleotideTests

open BioFSharp
open Nucleotides
open Expecto

let allNucs = [Nucleotides.A; Nucleotides.C; Nucleotides.T; Nucleotides.U
               Nucleotides.I; Nucleotides.Gap; Nucleotides.Ter; Nucleotides.R
               Nucleotides.Y; Nucleotides.K; Nucleotides.M; Nucleotides.S
               Nucleotides.W; Nucleotides.B; Nucleotides.D; Nucleotides.H
               Nucleotides.V; Nucleotides.N]
let allSymbols = ['A';'T';'C';'G';'U';'I';'-';'*';'R';'Y';'K';'M';'S';'W';'B';'D';'H';'V';'N']
let allFormuals = [Formula.table.A; Formula.table.C; Formula.table.T; Formula.table.U
                   Formula.table.I; Formula.emptyFormula; Formula.emptyFormula; Formula.emptyFormula
                   Formula.emptyFormula; Formula.emptyFormula; Formula.emptyFormula; Formula.emptyFormula
                   Formula.emptyFormula; Formula.emptyFormula; Formula.emptyFormula; Formula.emptyFormula
                   Formula.emptyFormula; Formula.emptyFormula]


[<Tests>]
let nucleotideTests = [
    testList "Nucleotides" [
        testCase "symbol" (fun() ->
            let testSymbols = allNucs |> List.Map(fun nuc -> Nucleotides.symbol nuc)
            Expect.equal
                testSymbols
                allSymbols
                "Nucleotides.symbol did not return the correct symbol for all Nucleotides."
        )
        testCase "formula" (fun() ->
            let testFormulas = allNucs |> List.Map(fun nuc -> Nucleotides.formula nuc)
            Expect.equal
                testFormulas
                allFormuals
                "Nucleotides.formula did not return the correct formula for all Nucleotides."
        )
        testCase "isTerminator" (fun() ->
            let testBools = [
                false; false; false; false; false; false; true; false; false
                false; false; false; false; false; false; false; false; false
            ]
            Expect.equal
                (allNucs |> List.Map(fun nuc -> Nucleotides.isTerminator nuc))
                testBools
                "Nucleotides.isTerminator did not return the correct boolean for all Nucleotides."
        )
    ]
]