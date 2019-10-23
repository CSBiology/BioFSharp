namespace BioFSharp.Tests

open Expecto
open BioFSharp

module BioArray =
    
    open AminoAcids
    
    let aminoAcidSet : BioArray.BioArray<AminoAcid> = 
        [|Ala;Cys;Asp;Glu;Phe;Gly;His;Ile;Lys;Leu;Met;Asn;Pyl;Pro;Gln;Arg;Ser;Thr;Sel;Val;Trp;Tyr;Xaa;Xle;Glx;Asx;Gap;Ter|]

    let aminoAcidSymbolSet : BioArray.BioArray<AminoAcidSymbols.AminoAcidSymbol> = 
        aminoAcidSet |> Array.map AminoAcidSymbols.aminoAcidSymbol

    [<Tests>]
    let parsingTests  =

        testList "Parsing.AminoAcids" [

            testCase "Parse full amino acid set" <| fun () ->

                let parsedAminoAcids =
                    "ACDEFGHIKLMNOPQRSTUVWYXJZB-*"
                    |> BioArray.ofAminoAcidString

                Expect.equal
                    aminoAcidSet
                    parsedAminoAcids
                    "BioArray.ofAminoAcidString did not parse the amino acid set correctly."

            testCase "Parse full amino acid symbol set" <| fun () ->
                        
                let parsedAminoAcidSymbols =
                    "ACDEFGHIKLMNOPQRSTUVWYXJZB-*"
                    |> BioArray.ofAminoAcidSymbolString

                Expect.equal 
                    aminoAcidSymbolSet
                    parsedAminoAcidSymbols
                    "BioArray.ofAminoAcidSymbolString did not parse the amino acid set correctly."
                
        ]
        
        
        

