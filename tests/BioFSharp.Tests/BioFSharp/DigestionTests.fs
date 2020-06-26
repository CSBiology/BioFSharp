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


    ///
    let testMotiv = 
        [|
            (His, [|None; None; None; Some His; Some His; Some Ile|])
            (His, [|None; None; Some His; Some His; Some Ile; Some Lys|])
            (Ile, [|None; Some His; Some His; Some Ile; Some Lys; Some Glu|])
            (Lys, [|Some His; Some His; Some Ile; Some Lys; Some Glu; Some Phe|])
            (Glu, [|Some His; Some Ile; Some Lys; Some Glu; Some Phe; None|])
            (Phe, [|Some Ile; Some Lys; Some Glu; Some Phe; None; None|])
        |]

    let testDigest = 
        [|
            {
                ProteinID = 1
                MissCleavages = 0
                CleavageStart = 0
                CleavageEnd = fstTestPeptide.Length-1
                PepSequence = fstTestPeptide |> Array.toList
            }
            { 
                ProteinID = 1
                MissCleavages = 0
                CleavageStart = fstTestPeptide.Length
                CleavageEnd = fstTestPeptide.Length+sndTestPeptide.Length-1
                PepSequence = sndTestPeptide |> Array.toList
            }
            { 
                ProteinID = 1
                MissCleavages = 0
                CleavageStart = fstTestPeptide.Length+sndTestPeptide.Length
                CleavageEnd = fstTestPeptide.Length+sndTestPeptide.Length+trdTestSequence.Length-1
                PepSequence = trdTestSequence |> Array.toList
            }
        |]

    let testMissCleavages = 
        [|
            // No MissCleavages
            { 
                ProteinID = 1
                MissCleavages = 0
                CleavageStart = fstTestPeptide.Length+sndTestPeptide.Length
                CleavageEnd = fstTestPeptide.Length+sndTestPeptide.Length+trdTestSequence.Length-1
                PepSequence = trdTestSequence |> Array.toList
            }
            { 
                ProteinID = 1
                MissCleavages = 0
                CleavageStart = fstTestPeptide.Length
                CleavageEnd = fstTestPeptide.Length+sndTestPeptide.Length-1
                PepSequence = sndTestPeptide |> Array.toList
            }
            {
                ProteinID = 1
                MissCleavages = 0
                CleavageStart = 0
                CleavageEnd = fstTestPeptide.Length-1
                PepSequence = fstTestPeptide |> Array.toList
            }
            // one MissCleavage
            { 
                ProteinID = 1
                MissCleavages = 1
                CleavageStart = 9
                CleavageEnd = fstTestPeptide.Length+sndTestPeptide.Length+trdTestSequence.Length-1
                PepSequence = List.concat [sndTestPeptide |> Array.toList;trdTestSequence |> Array.toList]
            }
            { 
                ProteinID = 1
                MissCleavages = 1
                CleavageStart = 0
                CleavageEnd = fstTestPeptide.Length+sndTestPeptide.Length-1
                PepSequence = List.concat [fstTestPeptide |> Array.toList;sndTestPeptide |> Array.toList]
            }
            // two MissCleavages
            { 
                ProteinID = 1
                MissCleavages = 2
                CleavageStart = 0
                CleavageEnd = fstTestPeptide.Length+sndTestPeptide.Length+trdTestSequence.Length-1
                PepSequence = List.concat [fstTestPeptide |> Array.toList;sndTestPeptide |> Array.toList;trdTestSequence |> Array.toList]
            }
        |]

    [<Tests>]
    let digestionTests  =
        
        testList "Digestion" [

            testList "isCuttingSite" [
                    
                testCase "isCuttingSite_lys" (fun () ->
                    let isCuttingSite_lys =
                        Digestion.isCuttingSite testProtease (trypsinTestCuttingSiteLys |> Array.map Some)

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
            testList "BioSeq" [
    
                testCase "motivy" (fun () ->
                    let motiv =
                        Digestion.BioSeq.motivy 3 2 (trypsinTestCuttingSiteLys)

                    Expect.sequenceEqual motiv testMotiv
                        "Motivy 3 2 did not yield the expected result"
                )
                testCase "digest" (fun () ->
                    let digest =
                        Digestion.BioSeq.digest testProtease 1 testSequence

                    Expect.sequenceEqual digest testDigest
                        "Digestion.BioSeq.digest failed to produce expected digested peptides."
                )
                testCase "concernMissCleavages" (fun () ->
                    let withMissCleavages =
                        Digestion.BioSeq.concernMissCleavages 0 2 testDigest

                    Expect.sequenceEqual withMissCleavages testMissCleavages
                        "Digestion.BioSeq.concernMissCleavages failed to produce expected digested peptides."
                )
            ]
            testList "BioArray" [
                
                testCase "motivy" (fun () ->
                    let motiv =
                        Digestion.BioArray.motivy 3 2 (trypsinTestCuttingSiteLys)

                    Expect.sequenceEqual motiv testMotiv
                        "Motivy 3 2 did not yield the expected result"
                )
                testCase "digest" (fun () ->
                    let digest =
                        Digestion.BioArray.digest testProtease 1 testSequence

                    Expect.sequenceEqual digest testDigest
                        "Digestion.BioArray.digest failed sto produce expected digested peptides."
                )
                testCase "concernMissCleavages" (fun () ->
                    let withMissCleavages =
                        Digestion.BioArray.concernMissCleavages 0 2 testDigest

                    Expect.sequenceEqual withMissCleavages testMissCleavages
                        "Digestion.BioArray.concernMissCleavages failed to produce the expected digested peptides."
                )
            ]

        ]

      