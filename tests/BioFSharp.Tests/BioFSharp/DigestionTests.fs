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
          
    let trypsinTestCuttingSiteLys =
        [|His;His;Ile;Lys;Glu;Phe|]
    
    let trypsinTestCuttingSiteArg =
        [|His;His;Ile;Arg;Glu;Phe|]

    let trypsinTestCuttingSiteLys_Pro =
        [|His;His;Ile;Lys;Pro;Phe|]
        
    let trypsinTestCuttingSiteArg_Pro =
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

            testList "isCuttingSite" [
                    
                testCase "isCuttingSite_lys" (fun () ->
                    let isCuttingSite_lys =
                        Digestion.isCutingSite testProtease (trypsinTestCuttingSiteLys |> Array.map Some)

                    Expect.isTrue isCuttingSite_lys
                        "Digestion.isCuttingSite did not identify the CuttingSite correctly."

                )
                testCase "isCuttingSite_Arg" (fun () ->
                    let isCuttingSite_Arg =
                        Digestion.isCuttingSite testProtease (trypsinTestCuttingSiteArg |> Array.map Some)

                    Expect.isTrue isCuttingSite_Arg
                        "Digestion.isCuttingSite did not identify the CuttingSite correctly."

                )
                testCase "isCuttingSite_lysPro" (fun () ->
                    let isCuttingSite_lysPro =
                        Digestion.isCuttingSite testProtease (trypsinTestCuttingSiteLys_Pro |> Array.map Some)
                        |> not 

                    Expect.isTrue isCuttingSite_lysPro
                        "Digestion.isCuttingSite did not identify the CuttingSite correctly."

                )
                testCase "isCuttingSite_argPro" (fun () ->
                    let isCuttingSite_argPro =
                        Digestion.isCuttingSite testProtease (trypsinTestCuttingSiteArg_Pro |> Array.map Some)
                        |> not 

                    Expect.isTrue isCuttingSite_argPro
                        "Digestion.isCuttingSite did not identify the CuttingSite correctly."

                )
                ]
            ]

      