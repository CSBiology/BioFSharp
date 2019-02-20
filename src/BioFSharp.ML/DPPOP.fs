namespace BioFSharp.ML

module DPPOP =
    
    open FSharpAux
    open FSharpAux.IO
    open BioFSharp
    open BioFSharp.IO
    open BioFSharp.Digestion
  
    ///
    module Classification =

        ///Tryptic digestion of an amino acid sequence with the ability to control the maximal amount of misscleavages and filtering of a minimal peptide length
        let digestTrypticWith (maxMissCleavages:int) (minPeptideLength:int) (aminoAcidSeq:BioArray.BioArray<AminoAcids.AminoAcid>) =
            Digestion.BioArray.digest (Digestion.Table.getProteaseBy "Trypsin") 0 aminoAcidSeq
            |> Digestion.BioArray.concernMissCleavages 0 maxMissCleavages
            |> Seq.map (fun p -> p.PepSequence |> List.toArray)
            |> Seq.filter (fun p -> p.Length > minPeptideLength)

        ///Tryptic digestion of an amino acid sequence with the settings used for the dppop web API
        let digestTryptic (aminoAcidSeq:BioArray.BioArray<AminoAcids.AminoAcid>) =
            aminoAcidSeq
            |> digestTrypticWith 3 6 

        ///
        let getDistinctTrypticPeptidesFromFasta (fa:seq<FastA.FastaItem<BioArray.BioArray<AminoAcids.AminoAcid>>>)= 
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


        let getDifestionEfficiency (protId) (sequence:BioArray.BioArray<AminoAcids.AminoAcid>) =
            let cleavageScore = sequence |> Array.map AminoAcidSymbols.aminoAcidSymbol |> Digestion.CleavageScore.calculateCleavageScore
        
            //TODO: digestion hast changed from 1 based index to 0 based index, identify the numbers to change
            let getStart index = if index < 2 then 0. else cleavageScore.[index-1]
            let getEnd index = if index >= cleavageScore.Length  then 0. else cleavageScore.[index]


            let calc (p:DigestedPeptide<int>) =
                if p.MissCleavages < 1 then
                    (protId,p.PepSequence |> Seq.map AminoAcidSymbols.aminoAcidSymbol |> Seq.toArray ),(getStart p.CleavageStart,0.,getEnd p.CleavageEnd)
                else
                    let inter' = p.MissCleavages - 1 |> float
                    let s = getStart p.CleavageStart                
                    let e = getEnd p.CleavageEnd
                    // let inter' = inter - s - e
                    (protId,p.PepSequence |> Seq.map AminoAcidSymbols.aminoAcidSymbol |> Seq.toArray),(s,inter',e)

            Digestion.BioArray.digest (Digestion.Table.getProteaseBy "Trypsin") 0 sequence
            |> Digestion.BioArray.concernMissCleavages 0 3
            |> Seq.map calc

    ///
    module Prediction =
        ()