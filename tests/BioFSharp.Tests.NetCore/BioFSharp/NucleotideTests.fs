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
    ]
]