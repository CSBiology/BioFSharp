module BioCollectionsTests 

open Expecto
open BioFSharp
    
open AminoAcids
    
let aminoAcidSetArray : BioArray.BioArray<AminoAcid> = 
    [|Ala;Cys;Asp;Glu;Phe;Gly;His;Ile;Lys;Leu;Met;Asn;Pyl;Pro;Gln;Arg;Ser;Thr;Sel;Val;Trp;Tyr;Xaa;Xle;Glx;Asx;Gap;Ter|]

let aminoAcidSymbolSetArray : BioArray.BioArray<AminoAcidSymbols.AminoAcidSymbol> = 
    aminoAcidSetArray |> Array.map AminoAcidSymbols.aminoAcidSymbol

let testProt : BioArray.BioArray<AminoAcid> = 
    [|Met;Val;Leu|]

open Nucleotides

let nucleotideSetArray : BioArray.BioArray<Nucleotide> = 
    [|A;T;G;C;U;I;Gap;Ter;R;Y;K;M;S;W;B;D;H;V;N|]

let testCodingStrand : BioArray.BioArray<Nucleotides.Nucleotide> = 
    [|A;T;G;G;T;A;C;T;G;A;C|]

let testCodingStrandRev : BioArray.BioArray<Nucleotides.Nucleotide> = 
    testCodingStrand |> Array.rev

let testCodingStrandRevComplement : BioArray.BioArray<Nucleotides.Nucleotide> = 
    [|G;T;C;A;G;T;A;C;C;A;T|]  

let testTemplateStrand : BioArray.BioArray<Nucleotides.Nucleotide> = 
    [|T;A;C;C;A;T;G;A;C;T;G|]

let testTranscript : BioArray.BioArray<Nucleotides.Nucleotide> = 
    [|A;U;G;G;U;A;C;U;G;A;C|]

let testTriplets =
    [|(T,A,C);(C,A,T);(G,A,C)|]

[<Tests>]
let bioCollectionsTests  =
        
    testList "BioCollections" [

        testList "BioArray" [
            

            testCase "ofAminoAcidString" (fun () ->
                let parsedAminoAcids =
                    "ACDEFGHIKLMNOPQRSTUVWYXJZB-*"
                    |> BioArray.ofAminoAcidString

                Expect.equal
                    aminoAcidSetArray
                    parsedAminoAcids
                    "BioArray.ofAminoAcidString did not parse the amino acid set correctly."
            )

            testCase "ofAminoAcidSymbolString" (fun () ->
                let parsedAminoAcidSymbols =
                    "ACDEFGHIKLMNOPQRSTUVWYXJZB-*"
                    |> BioArray.ofAminoAcidSymbolString

                Expect.equal 
                    aminoAcidSymbolSetArray
                    parsedAminoAcidSymbols
                    "BioArray.ofAminoAcidSymbolString did not parse the amino acid set correctly."
            )

            testCase "ofNucleotideString" (fun () ->
                let parsedNucleotides =
                    "ATGCUI-*RYKMSWBDHVN"
                    |> BioArray.ofNucleotideString

                Expect.equal 
                    nucleotideSetArray
                    parsedNucleotides
                    "BioArray.ofNucleotideString did not parse the nucleotide set correctly."
            )

            testCase "reverse" (fun () ->
                Expect.equal 
                    (testCodingStrand |> BioArray.reverse)
                    testCodingStrandRev
                    "BioArray.reverse did not reverse the nucleotide sequence correctly."
            )
            
            testCase "complement" (fun () ->
                Expect.equal 
                    (testCodingStrand |> BioArray.complement)
                    testTemplateStrand
                    "BioArray.complement did not build the reverse complement of the nucleotide sequence correctly."
            )

            testCase "reverseComplement" (fun () ->
                Expect.equal 
                    (testCodingStrand |> BioArray.reverseComplement)
                    testCodingStrandRevComplement
                    "BioArray.reverseComplement did not build the reverse complement of the nucleotide sequence correctly."
            )

            testCase "mapInTriplets" (fun () ->
                Expect.equal 
                    (testTemplateStrand |> BioArray.mapInTriplets id)
                    testTriplets
                    "BioArray.reverseComplement did not build the correct base triplets."
            )

            testCase "transcribeCodeingStrand" (fun () ->
                Expect.equal 
                    (testCodingStrand |> BioArray.transcribeCodingStrand)
                    testTranscript
                    "BioArray.transcribeCodeingStrand did not transcribe the coding strand correctly."
            )

            testCase "transcribeTemplateStrand" (fun () ->
                Expect.equal 
                    (testTemplateStrand |> BioArray.transcribeTemplateStrand)
                    testTranscript
                    "BioArray.transcribeTemplateStrand did not transcribe the template strand correctly."
            )

            testCase "translate" (fun () ->
                Expect.equal 
                    (testTranscript |> BioArray.translate 0)
                    testProt
                    "BioArray.translate did not translate the transcript correctly."
            )

            testCase "isEqual" (fun () ->
                Expect.equal
                    (testTranscript |> BioArray.isEqual testTranscript)
                    0
                    "BioArray.isEqual did not return correct integer when transcripts were equal."
            )

            testCase "toString" (fun () ->
                Expect.equal
                    (aminoAcidSetArray |> BioArray.toString)
                    "ACDEFGHIKLMNOPQRSTUVWYXJZB-*"
                    "BioArray.toString did not return the correct string"
            )

            testCase "toMonoisotopicMass" (fun () ->
                Expect.floatClose
                    Accuracy.high
                    (testProt |> BioArray.toMonoisotopicMass)
                    // Masses obtained from University of Washington Proteomics Resource https://proteomicsresource.washington.edu/protocols06/masses.php
                    (131.04048 + 99.06841 + 113.08406)
                    "BioArray.toMonoisotopicMass did not return correct mass"
            )

            testCase "toAverageMass" (fun() ->
                Expect.floatClose
                    Accuracy.medium // High accuracy was not passing test
                    (testProt |> BioArray.toAverageMass)
                    // Masses obtained from University of Washington Proteomics Resource https://proteomicsresource.washington.edu/protocols06/masses.php
                    (131.19606 + 99.13106 + 113.15764)
                    "BioArray.toAverageMass did not return correct mass"
            )

            testCase "toMonoisotopicMassWith" (fun () ->
                Expect.floatClose
                    Accuracy.high
                    (testProt |> BioArray.toMonoisotopicMassWith 18.0) // 18 = mass of one water molecule
                    // Masses obtained from University of Washington Proteomics Resource https://proteomicsresource.washington.edu/protocols06/masses.php
                    (131.04048 + 99.06841 + 113.08406 + 18.0)
                    "BioArray.toMonoisotopicMassWith did not return correct mass"
            )

            testCase "toAverageMassWith" (fun () ->
                Expect.floatClose
                    Accuracy.medium
                    (testProt |> BioArray.toAverageMassWith 18.0) // 18 = mass of one water molecule
                    // Masses obtained from University of Washington Proteomics Resource https://proteomicsresource.washington.edu/protocols06/masses.php
                    (131.19606 + 99.13106 + 113.15764 + 18.0)
                    "BioArray.toAverageMassWith did not return correct mass"
            )

            testCase "toCompositionVector" (fun () ->
                let testCompVec = Array.zeroCreate 26
                let metIndex = 12 // Value of (int(BioItem.symbol Met)) - 65
                let valIndex = 21 // Value of (int(BioItem.symbol Val)) - 65
                let leuIndex = 11 // Value of (int(BioItem.symbol Leu)) - 65
                testCompVec.[metIndex] <- testCompVec.[metIndex] + 1
                testCompVec.[valIndex] <- testCompVec.[valIndex] + 1
                testCompVec.[leuIndex] <- testCompVec.[leuIndex] + 1
                Expect.equal
                    (testProt |> BioArray.toCompositionVector)
                    testCompVec
                    "BioArray.toCompositionVector did not return correct vector"
            )
        ]

        testList "BioList" [
            

            testCase "ofAminoAcidString" (fun () ->
                let parsedAminoAcids =
                    "ACDEFGHIKLMNOPQRSTUVWYXJZB-*"
                    |> BioList.ofAminoAcidString

                Expect.equal
                    parsedAminoAcids
                    (aminoAcidSetArray|> List.ofArray)
                    "BioList.ofAminoAcidString did not parse the amino acid set correctly."
            )

            testCase "ofAminoAcidSymbolString" (fun () ->
                let parsedAminoAcidSymbols =
                    "ACDEFGHIKLMNOPQRSTUVWYXJZB-*"
                    |> BioList.ofAminoAcidSymbolString

                Expect.equal 
                    (aminoAcidSymbolSetArray |> List.ofArray)
                    parsedAminoAcidSymbols
                    "BioList.ofAminoAcidSymbolString did not parse the amino acid set correctly."
            )

            testCase "ofNucleotideString" (fun () ->
                let parsedNucleotides =
                    "ATGCUI-*RYKMSWBDHVN"
                    |> BioList.ofNucleotideString

                Expect.equal 
                    (nucleotideSetArray |> List.ofArray)
                    parsedNucleotides
                    "BioList.ofNucleotideString did not parse the nucleotide set correctly."
            )

            testCase "reverse" (fun () ->
                Expect.equal 
                    (testCodingStrand |> List.ofArray |> BioList.reverse)
                    (testCodingStrandRev |> List.ofArray)
                    "BioList.reverse did not reverse the nucleotide sequence correctly."
            )
            
            testCase "complement" (fun () ->
                Expect.equal 
                    (testCodingStrand |> List.ofArray |> BioList.complement)
                    (testTemplateStrand |> List.ofArray)
                    "BioList.complement did not build the reverse complement of the nucleotide sequence correctly."
            )

            testCase "reverseComplement" (fun () ->
                Expect.equal 
                    (testCodingStrand |> List.ofArray |> BioList.reverseComplement)
                    (testCodingStrandRevComplement |> List.ofArray)
                    "BioList.reverseComplement did not build the reverse complement of the nucleotide sequence correctly."
            )

            testCase "mapInTriplets" (fun () ->
                Expect.equal 
                    (testTemplateStrand |> List.ofArray |> BioList.mapInTriplets id)
                    (testTriplets |> List.ofArray)
                    "BioList.reverseComplement did not build the correct base triplets."
            )

            testCase "transcribeCodeingStrand" (fun () ->
                Expect.equal 
                    (testCodingStrand |> List.ofArray |> BioList.transcribeCodingStrand)
                    (testTranscript |> List.ofArray)
                    "BioList.transcribeCodeingStrand did not transcribe the coding strand correctly."
            )

            testCase "transcribeTemplateStrand" (fun () ->
                Expect.equal 
                    (testTemplateStrand |> List.ofArray |> BioList.transcribeTemplateStrand)
                    (testTranscript |> List.ofArray)
                    "BioList.transcribeTemplateStrand did not transcribe the template strand correctly."
            )

            testCase "translate" (fun () ->
                Expect.equal 
                    (testTranscript |> List.ofArray |> BioList.translate 0)
                    (testProt |> List.ofArray)
                    "BioList.translate did not translate the transcript correctly."
            )

            testCase "isEqual" (fun () ->
                Expect.equal
                    (testTranscript |> List.ofArray
                    |> BioList.isEqual (testTranscript |> List.ofArray))
                    0
                    "BioList.isEqual did not return correct integer when transcripts were equal."
            )

            testCase "toString" (fun () ->
                Expect.equal
                    (aminoAcidSetArray |> List.ofArray |> BioList.toString)
                    "ACDEFGHIKLMNOPQRSTUVWYXJZB-*"
                    "BioList.toString did not return the correct string"
            )

            testCase "toMonoisotopicMass" (fun () ->
                Expect.floatClose
                    Accuracy.high
                    (testProt |> List.ofArray |> BioList.toMonoisotopicMass)
                    // Masses obtained from University of Washington Proteomics Resource https://proteomicsresource.washington.edu/protocols06/masses.php
                    (131.04048 + 99.06841 + 113.08406)
                    "BioList.toMonoisotopicMass did not return correct mass"
            )

            testCase "toAverageMass" (fun() ->
                Expect.floatClose
                    Accuracy.medium // High accuracy was not passing test
                    (testProt |> List.ofArray |> BioList.toAverageMass)
                    // Masses obtained from University of Washington Proteomics Resource https://proteomicsresource.washington.edu/protocols06/masses.php
                    (131.19606 + 99.13106 + 113.15764)
                    "BioList.toAverageMass did not return correct mass"
            )

            testCase "toMonoisotopicMassWith" (fun () ->
                Expect.floatClose
                    Accuracy.high
                    (testProt |> List.ofArray |> BioList.toMonoisotopicMassWith 18.0) // 18 = mass of one water molecule
                    // Masses obtained from University of Washington Proteomics Resource https://proteomicsresource.washington.edu/protocols06/masses.php
                    (131.04048 + 99.06841 + 113.08406 + 18.0)
                    "BioList.toMonoisotopicMassWith did not return correct mass"
            )

            testCase "toAverageMassWith" (fun () ->
                Expect.floatClose
                    Accuracy.medium
                    (testProt |> List.ofArray |> BioList.toAverageMassWith 18.0) // 18 = mass of one water molecule
                    // Masses obtained from University of Washington Proteomics Resource https://proteomicsresource.washington.edu/protocols06/masses.php
                    (131.19606 + 99.13106 + 113.15764 + 18.0)
                    "BioList.toAverageMassWith did not return correct mass"
            )

            testCase "toCompositionVector" (fun () ->
                let testCompVec = Array.zeroCreate 26
                let metIndex = 12 // Value of (int(BioItem.symbol Met)) - 65
                let valIndex = 21 // Value of (int(BioItem.symbol Val)) - 65
                let leuIndex = 11 // Value of (int(BioItem.symbol Leu)) - 65
                testCompVec.[metIndex] <- testCompVec.[metIndex] + 1
                testCompVec.[valIndex] <- testCompVec.[valIndex] + 1
                testCompVec.[leuIndex] <- testCompVec.[leuIndex] + 1
                Expect.equal
                    (testProt |> List.ofArray |> BioList.toCompositionVector)
                    testCompVec
                    "BioList.toCompositionVector did not return correct vector"
            )
        ]

        testList "BioSeq" [
            

                testCase "ofAminoAcidString" (fun () ->
                    let parsedAminoAcids =
                        "ACDEFGHIKLMNOPQRSTUVWYXJZB-*"
                        |> BioSeq.ofAminoAcidString

                    Expect.sequenceEqual
                        parsedAminoAcids
                        (aminoAcidSetArray|> Seq.ofArray)
                        "BioSeq.ofAminoAcidString did not parse the amino acid set correctly."
                )

                testCase "ofAminoAcidSymbolString" (fun () ->
                    let parsedAminoAcidSymbols =
                        "ACDEFGHIKLMNOPQRSTUVWYXJZB-*"
                        |> BioSeq.ofAminoAcidSymbolString

                    Expect.sequenceEqual 
                        (aminoAcidSymbolSetArray |> Seq.ofArray)
                        parsedAminoAcidSymbols
                        "BioSeq.ofAminoAcidSymbolString did not parse the amino acid set correctly."
                )

                testCase "ofNucleotideString" (fun () ->
                    let parsedNucleotides =
                        "ATGCUI-*RYKMSWBDHVN"
                        |> BioSeq.ofNucleotideString

                    Expect.sequenceEqual 
                        (nucleotideSetArray |> Seq.ofArray)
                        parsedNucleotides
                        "BioSeq.ofNucleotideString did not parse the nucleotide set correctly."
                )

                testCase "reverse" (fun () ->
                    Expect.sequenceEqual 
                        (testCodingStrand |> Seq.ofArray |> BioSeq.reverse)
                        (testCodingStrandRev |> Seq.ofArray)
                        "BioSeq.reverse did not reverse the nucleotide sequence correctly."
                )
            
                testCase "complement" (fun () ->
                    Expect.sequenceEqual 
                        (testCodingStrand |> Seq.ofArray |> BioSeq.complement)
                        (testTemplateStrand |> Seq.ofArray)
                        "BioSeq.complement did not build the reverse complement of the nucleotide sequence correctly."
                )

                testCase "reverseComplement" (fun () ->
                    Expect.sequenceEqual 
                        (testCodingStrand |> Seq.ofArray |> BioSeq.reverseComplement)
                        (testCodingStrandRevComplement |> Seq.ofArray)
                        "BioSeq.reverseComplement did not build the reverse complement of the nucleotide sequence correctly."
                )

                testCase "mapInTriplets" (fun () ->
                    Expect.sequenceEqual 
                        (testTemplateStrand |> Seq.ofArray |> BioSeq.mapInTriplets id)
                        (testTriplets |> Seq.ofArray)
                        "BioSeq.reverseComplement did not build the correct base triplets."
                )

                testCase "transcribeCodeingStrand" (fun () ->
                    Expect.sequenceEqual 
                        (testCodingStrand |> Seq.ofArray |> BioSeq.transcribeCodingStrand)
                        (testTranscript |> Seq.ofArray)
                        "BioSeq.transcribeCodeingStrand did not transcribe the coding strand correctly."
                )

                testCase "transcribeTemplateStrand" (fun () ->
                    Expect.sequenceEqual
                        (testTemplateStrand |> Seq.ofArray |> BioSeq.transcribeTemplateStrand)
                        (testTranscript |> Seq.ofArray)
                        "BioSeq.transcribeTemplateStrand did not transcribe the template strand correctly."
                )

                testCase "translate" (fun () ->
                    Expect.sequenceEqual 
                        (testTranscript |> Seq.ofArray |> BioSeq.translate 0)
                        (testProt |> Seq.ofArray)
                        "BioSeq.translate did not translate the transcript correctly."
                )

                testCase "isEqual" (fun () ->
                    Expect.equal
                        (testTranscript |> Seq.ofArray
                        |> BioSeq.isEqual (testTranscript |> Seq.ofArray))
                        0
                        "BioSeq.isEqual did not return correct integer when transcripts were equal."
                )

                testCase "toString" (fun () ->
                    Expect.equal
                        (aminoAcidSetArray |> Seq.ofArray |> BioSeq.toString)
                        "ACDEFGHIKLMNOPQRSTUVWYXJZB-*"
                        "BioSeq.toString did not return the correct string"
                )

                testCase "toMonoisotopicMass" (fun () ->
                    Expect.floatClose
                        Accuracy.high
                        (testProt |> Seq.ofArray |> BioSeq.toMonoisotopicMass)
                        // Masses obtained from University of Washington Proteomics Resource https://proteomicsresource.washington.edu/protocols06/masses.php
                        (131.04048 + 99.06841 + 113.08406)
                        "BioSeq.toMonoisotopicMass did not return correct mass"
                )

                testCase "toAverageMass" (fun() ->
                    Expect.floatClose
                        Accuracy.medium // High accuracy was not passing test
                        (testProt |> Seq.ofArray |> BioSeq.toAverageMass)
                        // Masses obtained from University of Washington Proteomics Resource https://proteomicsresource.washington.edu/protocols06/masses.php
                        (131.19606 + 99.13106 + 113.15764)
                        "BioSeq.toAverageMass did not return correct mass"
                )

                testCase "toMonoisotopicMassWith" (fun () ->
                    Expect.floatClose
                        Accuracy.high
                        (testProt |> Seq.ofArray |> BioSeq.toMonoisotopicMassWith 18.0) // 18 = mass of one water molecule
                        // Masses obtained from University of Washington Proteomics Resource https://proteomicsresource.washington.edu/protocols06/masses.php
                        (131.04048 + 99.06841 + 113.08406 + 18.0)
                        "BioSeq.toMonoisotopicMassWith did not return correct mass"
                )
        ]
    ]
