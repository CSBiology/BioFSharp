namespace BioFSharp.ML

module DPPOP =
    open System.Collections.Generic
    open FSharpAux
    open FSharpAux.IO
    open BioFSharp
    open BioFSharp.IO
    open BioFSharp.BioArray
    open BioFSharp.Digestion
    open BioFSharp.AminoAcidSymbols
    open CNTK

    // Input
    type Qid = {
        Id        : int
        Rank      : int
        Data      : float[]
        ProtId    : string
        Intensity : float
        Sequence  : string
    }

    // Output
    type ScoredQid = {
        Id        : int
        Rank      : int
        Data      : float[]
        ProtId    : string
        Intensity : float
        Sequence  : string
        Score     : float
        }

    let createScoredID id rank data protID intensity sequence score = {
        Id        = id
        Rank      = rank
        Data      = data
        ProtId    = protID
        Intensity = intensity
        Sequence  = sequence
        Score     = score
        }
    ///
    module Classification =

        let private peptideFeatures =
            [|
                AminoProperties.initGetAminoProperty AminoProperties.AminoProperty.ActivationGibbsEnergy9;
                AminoProperties.initGetAminoProperty AminoProperties.AminoProperty.MEMofSingleSpanning;
                AminoProperties.initGetAminoProperty AminoProperties.AminoProperty.PrincipalComponentII;
                AminoProperties.initGetAminoProperty AminoProperties.AminoProperty.HydrophobicityIndex2;        
                AminoProperties.initGetAminoProperty AminoProperties.AminoProperty.ChouFasmanCoil;
                AminoProperties.initGetAminoProperty AminoProperties.AminoProperty.AverageNumberSurroundingResidues;
                AminoProperties.initGetAminoProperty AminoProperties.AminoProperty.CompositionIntracellular;
                AminoProperties.initGetAminoProperty AminoProperties.AminoProperty.WeightsHelixMinus3;
                AminoProperties.initGetAminoProperty AminoProperties.AminoProperty.HelixFormationParameters;        
                AminoProperties.initGetAminoProperty AminoProperties.AminoProperty.FreeEnergyHelicalRegion;
                AminoProperties.initGetAminoProperty AminoProperties.AminoProperty.ELi;        
                AminoProperties.initGetAminoProperty AminoProperties.AminoProperty.CompositionExtracellular;
                AminoProperties.initGetAminoProperty AminoProperties.AminoProperty.HydrophobicityIndex;
            |]


        ///Tryptic digestion of an amino acid sequence with the ability to control the maximal amount of misscleavages and filtering of a minimal peptide length
        let digestTrypticWith (maxMissCleavages:int) (minPeptideLength:int) (aminoAcidSeq:BioArray<AminoAcids.AminoAcid>) =
            Digestion.BioArray.digest (Digestion.Table.getProteaseBy "Trypsin") 0 aminoAcidSeq
            |> Digestion.BioArray.concernMissCleavages 0 maxMissCleavages
            |> Seq.map (fun p -> p.PepSequence |> List.toArray)
            |> Seq.filter (fun p -> p.Length > minPeptideLength)

        ///Tryptic digestion of an amino acid sequence with the settings used for the dppop web API
        let digestTryptic (aminoAcidSeq:BioArray<AminoAcids.AminoAcid>) =
            aminoAcidSeq
            |> digestTrypticWith 3 6 

        ///
        let getDistinctTrypticPeptidesFromFasta (fa:seq<FastA.FastaItem<BioArray<AminoAcids.AminoAcid>>>)= 
            //fileDir + "Chlamy_Cp.fastA"
            fa
            |> Seq.map (fun fi -> fi.Sequence |> Array.filter (not << AminoAcids.isTerminator))
            |> Seq.collect digestTryptic
            |> Seq.map BioArray.toString
            |> Set.ofSeq

        ///
        let getDistinctTrypticPeptidesFromFastaFile (filePath: string) = 
            //fileDir + "Chlamy_Cp.fastA"
            filePath
            |> FastA.fromFile BioArray.ofAminoAcidString
            |> getDistinctTrypticPeptidesFromFasta


        let getDifestionEfficiency (protId) (sequence:BioArray<AminoAcids.AminoAcid>) =
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

        let createDigestionEfficiencyMapFromFasta (fa:seq<FastA.FastaItem<BioArray<AminoAcids.AminoAcid>>>) = 
            fa
            |> Seq.map (fun fi -> {fi with Sequence=fi.Sequence |> Array.filter (not << AminoAcids.isTerminator)})
            |> Seq.collect (fun fi -> getDifestionEfficiency fi.Header fi.Sequence)
            |> Map.ofSeq

        ///get the physicochemical properties of a peptide: length, MolecularWeight, NetCharge, PositiveCharge, NegativeCharge, piI, Relative frewuencies of polar, hydrophobic, and negatively charge amino acids
        let getPhysicochemicalProperties (peptide:BioArray<AminoAcidSymbol>) =
            let pI peptide = 
                //default function for pKr of charged aminoacids
                let pKrFunction = IsoelectricPoint.getpKr
                match IsoelectricPoint.tryFind pKrFunction 0.5 peptide with
                | Some (pk) -> pk
                | None -> 0.

            let len = float peptide.Length
            let positiveCharge = peptide |> Seq.countIf AminoAcidSymbols.isPosCharged |> float
            let negativeCharge = peptide |> Seq.countIf AminoAcidSymbols.isNegCharged |> float
            [|
                //length
                len;
                //MolecularWeight
                BioArray.toAverageMass peptide
                //  NetCharge
                negativeCharge + positiveCharge
                // PositiveCharge, 
                positiveCharge
                // NegativeCharge        
                negativeCharge
                // piI
                pI peptide      
                //RelFreqPolar
                peptide |> Seq.countIf AminoAcidSymbols.isPolar |> fun x -> float x / len  
                //RelFreqHydrophobic
                peptide |> Seq.countIf AminoAcidSymbols.isHydrophobic |> fun x -> float x / len
                //RelFreqNegative 
                negativeCharge / len
            |]


        ///get all features of a peptide used for classification in dppop given a map that maps from (proteinID,Sequence) -> . 
        let getPeptideFeatures (digestionEfficiencyMap:Map<(string*BioArray<AminoAcidSymbol>),(float*float*float)>) (protId:string) peptide =
            let getIndex (a:AminoAcidSymbol) = (int a) - 65
            // Relative amino acid frequency peptide features
            let relFreq = 
                let tmp = BioArray.toRelCompositionVector peptide
                [|
                    tmp.[getIndex AminoAcidSymbol.Ala];tmp.[getIndex AminoAcidSymbol.Cys];tmp.[getIndex AminoAcidSymbol.Asp];tmp.[getIndex AminoAcidSymbol.Glu];
                    tmp.[getIndex AminoAcidSymbol.Phe];tmp.[getIndex AminoAcidSymbol.Gly];tmp.[getIndex AminoAcidSymbol.His];tmp.[getIndex AminoAcidSymbol.Ile];
                    tmp.[getIndex AminoAcidSymbol.Lys];tmp.[getIndex AminoAcidSymbol.Leu];tmp.[getIndex AminoAcidSymbol.Met];tmp.[getIndex AminoAcidSymbol.Asn];
                    tmp.[getIndex AminoAcidSymbol.Pro];tmp.[getIndex AminoAcidSymbol.Gln];tmp.[getIndex AminoAcidSymbol.Arg];tmp.[getIndex AminoAcidSymbol.Ser];
                    tmp.[getIndex AminoAcidSymbol.Thr];tmp.[getIndex AminoAcidSymbol.Val];tmp.[getIndex AminoAcidSymbol.Trp];tmp.[getIndex AminoAcidSymbol.Tyr];        
                |]

            let physicochemical = getPhysicochemicalProperties peptide
            let pf = 
                peptideFeatures
                |> Array.map (fun f -> peptide |> Array.averageBy f)


            let digest = 
                if digestionEfficiencyMap.ContainsKey (protId,peptide) then
                    let a,b,c = digestionEfficiencyMap.[(protId,peptide)]
                    Some [|a;b;c|]
                else
                    //printfn "%s - %A" protId peptide
                    // [|0.;0.;0.|]
                    None
            match digest with
            | Some v -> Array.concat [|relFreq;physicochemical;pf;v|] |> Some
            | None -> None


    ///
    module Prediction =

        /// Loads a trained CNTK model (at path given by "model") and evaluates the scores for the given collection of qids (input)
        // in the final pipeline the parameter model is probably better modeled by a unionCase ;)
        let scoreBy (model:string) (data:Qid []) = 
            let device = DeviceDescriptor.CPUDevice

            let PeptidePredictor : Function = 
                Function.Load(model,device)

            ///////////Input 
            let inputVar: Variable = PeptidePredictor.Arguments.Item 0

            let inputShape = inputVar.Shape
            /// Gets Size of one Feature Vector
            let featureVectorLength = inputShape.[0] 

            /// Extracts all Features and appends them, stores Values in a List
            let featureData = 
                let tmp = new System.Collections.Generic.List<float32>()
                data |> Array.iter (fun x -> 
                                    let data' = x.Data |> Array.map (fun x -> float32 (x))
                                    tmp.AddRange(data')
                                   )
                tmp

            /// Creates an input Batch
            let inputValues = Value.CreateBatch(inputShape,featureData,device)

            let inputMap = new Dictionary<Variable,Value>()
            inputMap.Add(inputVar,inputValues)

            ///////////Output
            let outputVar : Variable = PeptidePredictor.Output

            let outputMap = new Dictionary<Variable,Value>()
            outputMap.Add(outputVar,null)

            PeptidePredictor.Evaluate(inputMap,outputMap,device)

            let outputValues = outputMap.[outputVar]

            let preds = 
                outputValues.GetDenseData<float32>(outputVar)
                |> Seq.concat
                |> Array.ofSeq

            let res = 
                Array.map2 (fun (data:Qid) preds -> createScoredID data.Id data.Rank data.Data data.ProtId data.Intensity data.Sequence (float preds)) data preds
            res