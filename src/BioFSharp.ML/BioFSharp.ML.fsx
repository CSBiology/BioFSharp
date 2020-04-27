#load "CNTKLoadscript.fsx"
open CNTKLoadscript

CNTKLoadscript.resolveCNTKDependencies ()

#r @"..\..\packages\FSharpAux\lib\netstandard2.0\FSharpAux.dll"
#r @"..\..\packages\FSharpAux.IO\lib\netstandard2.0\FSharpAux.IO.dll"
#r @"..\..\packages\FSharp.Stats\lib\netstandard2.0\FSharp.Stats.dll"
#I @"..\..\bin\BioFSharp.ML\netstandard2.0"
#r @"BioFSharp.dll"
#r "BioFSharp.IO"
#r "BioFSharp.ML"
//#load "DPPOP.fs"

open BioFSharp.ML.DPPOP
open BioFSharp.IO
open BioFSharp
open BioFSharp.IO.FastA
open FSharpAux
open FSharpAux.IO

//let tairIds = 
//    [
//    "AT5G05730.1"
//    "AT5G05590.2"
//    "AT1G29410.1"
//    "AT4G27070.1"
//    "AT4G13260.1"
//    "AT2G27150.1"
//    "AT1G04580.1"
//    "AT3G44310.1"
//    "AT3G44300.1"
//    "AT5G22300.1"
//    "AT1G18590.1"
//    "AT1G16410.1"
//    "AT1G16400.1"
//    "AT3G26830.1"
//    "AT2G30750.1"
//    "AT1G26380.1"
//    "AT4G31970.1"
//    "AT3G53260.1"
//    "AT5G04230.1"
//    "AT3G21230.1"
//    "AT4G37390.1"
//    "AT4G27260.1"
//    "AT5G54510.1"
//    "AT5G13320.1"
//    "AT5G13360.1"
//    "AT1G28130.1"
//    "AT3G02875.1"
//    "AT2G46370.1" 
//    "AT1G51760.1"
//    "AT5G56650.1"
//    "AT5G56660.1"
//    "AT1G51780.1"
//    "AT5G05600.1"
//    "AT2G44810.1"
//    "AT1G72520.1"
//    "AT1G67560.1"
//    "AT3G25780.1"
//    "AT2G06050.2"
//    "AT2G35690.1"
//    "AT1G23080.1" 
//    "AT3G26810.1"
//    "AT1G80490.1"
//    "AT5G27030.1"
//    "AT4G28910.2"
//    "AT1G19180.1"
//    "AT4G02570.1"
//    "AT5G20570.1"
//    "AT1G33410.2"
//    "AT1G80680.1"
//    "AT4G35580.1"
//    "AT1G58100.1" 
//    "AT1G74710.1" 
//    "AT2G43840.1"
//    "AT4G26200.1"
//    "AT2G05100.1"
//    "AT2G05070.1"
//    "AT4G02630.1"
//    "AT3G54050.1"
//    "AT3G20440.1"
//    "AT1G29910.1"
//    "AT3G52720.1"
//    "AT5G18200.1"
//    "AT1G49380.1"
//    "AT4G39120.1"
//    "AT1G71180.1"
//    "AT5G64380.1"
//    "AT1G30120.1"
//    "AT4G28706.1"
//    "AT3G15640.2"
//    "AT5G24300.1"
//    "AT4G09520.1"
//    "AT2G18700.1"
//    "AT5G11920.1"
//    "AT5G40650.1"
//    "AT3G14940.1"
//    "AT4G05020.2"
//    "AT4G15530.3"
//    "AT5G36120.1"
//    "AT1G44170.1"
//    "AT1G70730.1"
//    "AT3G55650.1"
//    "AT1G53310.1"
//    "AT3G02360.2"
//    "AT1G54100.1"
//    "AT1G50460.1"
//    "AT4G26390.1"
//    "AT5G65690.1"
//    "AT5G11110.1"
//    "AT3G43190.1"
//    "AT3G22370.1"
//    "AT1G78580.1"
//    "AT1G11720.1"
//    "AT1G08940.1"

//    ]

//let tairProteome = FastA.fromFile BioArray.ofAminoAcidString @"C:\Users\Kevin\Downloads\TAIR10_pep_20101214_updated.fasta"
//["a"] |> Seq.append ["b"]
//let pois =
//    tairIds
//    |> Seq.map (fun p -> tairProteome |> Seq.find (fun x -> x.Header.Contains(p)))

//let res = Prediction.scoreDppopPlant (tairProteome) (pois) |> List.ofSeq
//res
//|> Seq.concat
//|> Seq.groupBy (fun x -> x.ProteinId)
//|> Seq.map (fun (pId,res) -> res |> Seq.map (fun x -> x.ProteinId,x.Sequence,x.PredictionScore,x.Distinct))
//|> Seq.concat
//|> Seq.toCSV "\t" false
//|> Seq.append ["ProteinId\tPeptideSequence\tRelativeObservabilityScore\tDistinctPeptide"]
//|> Seq.write @"C:\Users\Kevin\Desktop\Tair10_prediction_results_final.txt"

//List.map2 
//    (fun a b -> if (not (a=b)) then printfn "%s is not %s" a b else printfn "same"               )
//    (Seq.fromFile @"C:\Users\Kevin\Desktop\Tair10_prediction_results_distinct.txt" |> List.ofSeq)
//    (Seq.fromFile @"C:\Users\Kevin\Desktop\Tair10_prediction_results_final.txt" |> List.ofSeq)

//let getDistinctTrypticPeptidesFromFasta (fa:seq<FastA.FastaItem<BioArray.BioArray<AminoAcids.AminoAcid>>>)= 
//    //fileDir + "Chlamy_Cp.fastA"
//    fa
//    |> Seq.map (fun fi -> fi.Sequence |> Array.filter (not << AminoAcids.isTerminator))
//    |> Seq.collect Classification.digestTryptic
//    |> Seq.map BioArray.toString
//    |> Set.ofSeq

//let getDistinctTrypticPeptidesFromFasta2 (fa:seq<FastA.FastaItem<BioArray.BioArray<AminoAcids.AminoAcid>>>)= 
//            //fileDir + "Chlamy_Cp.fastA"
//            fa
//            |> Seq.map (fun fi -> fi.Sequence |> Array.filter (not << AminoAcids.isTerminator))
//            |> Seq.collect Classification.digestTryptic
//            |> Seq.map BioArray.toString
//            |> Seq.countBy id
//            |> Seq.filter (fun (key,count) -> count=1)
//            |> Seq.map fst
//            |> Set.ofSeq

//let a = getDistinctTrypticPeptidesFromFasta tairProteome
//let b = Classification.getDistinctTrypticPeptidesFromFasta tairProteome

//Set.filter (fun x -> not (Set.contains x b)) a


let chlamyProteome = 
    FastA.fromFile BioArray.ofAminoAcidString @"C:\Users\Kevin\Desktop\Chlamy_JGI5_5.fasta"
    |> List.ofSeq

let testProt = List.find (fun (x:FastaItem<'a>) -> x.Header = @"Cre02.g120100.t1.2 Ribulose-1,5-bisphosphate carboxylase/oxygenase small subunit 1, chloroplast precursor ALS=RBCS1 DBV=JGI5.5 GN=RBCS1 OS=Chlamydomonas reinhardtii SV=2 TOU=Cre") chlamyProteome

let dppopPremadeTest = Prediction.scoreDppopPlant chlamyProteome [testProt] |> Array.ofSeq

open BioFSharp.ML.CNTKExtensions
open CNTK


let distinctPeptides = Classification.getDistinctTrypticPeptidesFromFasta chlamyProteome
let digestionEfficiencyMap = Classification.createDigestionEfficiencyMapFromFasta [testProt]
let digested =
    Classification.digestTryptic testProt.Sequence
    |> Seq.map (fun x -> BioArray.toString x)
    |> List.ofSeq

let testFeatures = 
    digested
    |> Seq.map (fun p -> Classification.getPeptideFeatures digestionEfficiencyMap testProt.Header (BioArray.ofAminoAcidSymbolString p))
    |> Array.ofSeq
    |> Array.choose id
    |> Array.map (Classification.zNormalizePlantFeatureVector)


let device = DeviceDescriptor.CPUDevice

let predictor = 
    loadModelWithDevice device @"C:\Users\Kevin\source\repos\CSBiology\BioFSharp\src\BioFSharp.ML\Resources\Chlamy5Times128.model"

let inputBatch = toInputBatchWithDevice device predictor (testFeatures |> Array.map (fun x -> x.Data |> Array.map float32))

let res = 
    let preds = 
        predictAsWithDevice<float32> device predictor inputBatch
    Array.map2 
        (fun (data:PredictionInput) preds -> 
            createPredictionOutput data.ProtId data.Sequence (float preds) (distinctPeptides.Contains(data.Sequence))
        ) 
        testFeatures 
        (preds |> Array.ofSeq)
    |> Array.sortByDescending (fun (x) -> x.PredictionScore)
    |> fun x -> let max = (Array.maxBy (fun (x) -> x.PredictionScore) x).PredictionScore 
                x |> Array.map (fun (x) -> if x.PredictionScore >= 0. then {x with PredictionScore = (x.PredictionScore/max)} else {x with PredictionScore = 0.0})


Array.map2
    
    (fun (s1,score1) (s2,score2) -> 
        printfn "ExtensionResult: %s : %f" s1 score1
        printfn "PremadeResult: %s : %f" s2 score2
        if (s1 = s2) && (score1 = score2) then
            printfn "ARE EQUAL"
        else
            printfn "ARE NOT EQUAL WHAAAAAAAAAAAAAAAAAAAAAAAT"

    )
    (
    res
    |> Array.map (fun x -> x.Sequence,x.PredictionScore)
    )
    (
    dppopPremadeTest
    |> Array.item 0
    |> Array.map (fun x -> x.Sequence,x.PredictionScore)
    )

//Example for paper


open BioFSharp.AminoProperties

let extractPeptideFeatures peptide =
    [|
        initGetAminoProperty AminoProperty.HydrophobicityIndex;
        initGetAminoProperty AminoProperty.AverageNumberSurroundingResidues;
        //...
    |] |> Array.map (fun f -> peptide |> Array.averageBy f)

let proteinsOfInterest = FastA.fromFile BioArray.ofAminoAcidSymbolString @"PATH_TO_PROTEINS\Proteins.fasta" |> Array.ofSeq

//let featureVectors = proteinsOfInterest |> Array.map ( fun fastaEntry -> fastaEntry.Sequence |> extractPeptideFeatures)

//let predictor' = loadPredictorDefault @"PATH_TO_MODEL\Model.model"

//let inputBatch' = createInputBatchDefault predictor featureVectors

//let predictionOutput = predictAsDefault<float32> predictor inputBatch