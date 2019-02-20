namespace BioFSharp.ML

module DPPOP =
    
    open FSharpAux
    open FSharpAux.IO
    open BioFSharp
    open BioFSharp.IO
  
    ///
    module Classification =

        ///
        let digestTryptic (aminoAcidSeq:BioArray.BioArray<AminoAcids.AminoAcid>) =
            Digestion.BioArray.digest (Digestion.Table.getProteaseBy "Trypsin") 0 aminoAcidSeq
            |> Digestion.BioArray.concernMissCleavages 0 3
            |> Seq.map (fun p -> p.PepSequence |> List.toArray) // TODO not |> List.toArray
            |> Seq.filter (fun p -> p.Length > 6)

        ///
        let getDistinctPeptidesFromFasta (fa:seq<FastA.FastaItem<BioArray.BioArray<AminoAcids.AminoAcid>>>)= 
            //fileDir + "Chlamy_Cp.fastA"
            fa
            |> Seq.map (fun fi -> fi.Sequence |> Array.filter (not << AminoAcids.isTerminator))
            |> Seq.collect digestTryptic
            |> Seq.map BioArray.toString
            |> Set.ofSeq

        ///
        let getDistinctPeptidesFromFastaFile (filePath: string) = 
            //fileDir + "Chlamy_Cp.fastA"
            filePath
            |> FastA.fromFile BioArray.ofAminoAcidString
            |> Seq.map (fun fi -> fi.Sequence |> Array.filter (not << AminoAcids.isTerminator))
            |> Seq.collect digestTryptic
            |> Seq.map BioArray.toString
            |> Set.ofSeq

    ///
    module Prediction =
        ()