namespace BioFSharp.Tests

open BioFSharp
open Expecto

module DigestionTests =

    open AminoAcids
    open Digestion

    let testProtease =
        createProtease "Trypsin" (let _p1 = [AminoAcid.Lys;AminoAcid.Arg] |> Set.ofList 
                                  fun p4 p3 p2 p1 p1' p2' -> 
                                  match p1,p1' with
                                  | Some a1,Some a1' -> _p1.Contains(a1) && not (a1' = AminoAcid.Pro)
                                  | _   -> false                     
                                  )   
          
    let trypsinTestCuttingSideLys =
        [|His;His;Ile;Lys;Glu;Phe|]
    
    let trypsinTestCuttingSideArg =
        [|His;His;Ile;Arg;Glu;Phe|]

    let trypsinTestCuttingSideLys_Pro =
        [|His;His;Ile;Lys;Pro;Phe|]
        
    let trypsinTestCuttingSideArg_Pro =
        [|His;His;Ile;Arg;Pro;Phe|]

    let fstTestPeptide : BioArray.BioArray<AminoAcid> = 
        [|Ala;Cys;Asp;Glu;Phe;Gly;His;Ile;Lys;|]

    let sndTestPeptide : BioArray.BioArray<AminoAcid> = 
        [|Leu;Met;Asn;Pyl;Pro;Gln;Arg;|]

    let trdTestSequence : BioArray.BioArray<AminoAcid> =
        [|Ser;Thr;Sel;Val;Trp;Tyr;Xaa;Xle;Glx;Asx;|]

    let testSequence = 
        Array.concat [|fstTestPeptide;sndTestPeptide;trdTestSequence|]

    [<Tests>]
    let digestionTests  =
        
        testList "Digestion" [

            testList "isCuttingSide" [
                    
                testCase "isCuttingSide_lys" (fun () ->
                    let isCuttingSide_lys =
                        Digestion.isCutingSite testProtease (trypsinTestCuttingSideLys |> Array.map Some)

                    Expect.isTrue isCuttingSide_lys
                        "Digestion.isCutingSite did not identify the cuttingside correctly."

                )
                testCase "isCuttingSide_Arg" (fun () ->
                    let isCuttingSide_Arg =
                        Digestion.isCutingSite testProtease (trypsinTestCuttingSideArg |> Array.map Some)

                    Expect.isTrue isCuttingSide_Arg
                        "Digestion.isCutingSite did not identify the cuttingside correctly."

                )
                testCase "isCuttingSide_lysPro" (fun () ->
                    let isCuttingSide_lysPro =
                        Digestion.isCutingSite testProtease (trypsinTestCuttingSideLys_Pro |> Array.map Some)
                        |> not 

                    Expect.isTrue isCuttingSide_lysPro
                        "Digestion.isCutingSite did not identify the cuttingside correctly."

                )
                testCase "isCuttingSide_argPro" (fun () ->
                    let isCuttingSide_argPro =
                        Digestion.isCutingSite testProtease (trypsinTestCuttingSideArg_Pro |> Array.map Some)
                        |> not 

                    Expect.isTrue isCuttingSide_argPro
                        "Digestion.isCutingSite did not identify the cuttingside correctly."

                )
                ]
            ]

      