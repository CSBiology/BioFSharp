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

type Qid = {
    Id        : int
    Rank      : int
    Data      : float[]
    ProtId    : string
    Intensity : float
    Sequence  : string
}


let chlamyProteome = 
    FastA.fromFile BioArray.ofAminoAcidString @"C:\Users\Kevin\Desktop\Chlamy_JGI5_5.fasta"
    |> List.ofSeq

let distinctPeptides = Classification.getDistinctTrypticPeptidesFromFasta chlamyProteome

let testProt = List.find (fun (x:FastaItem<'a>) -> x.Header = @"Cre02.g120100.t1.2 Ribulose-1,5-bisphosphate carboxylase/oxygenase small subunit 1, chloroplast precursor ALS=RBCS1 DBV=JGI5.5 GN=RBCS1 OS=Chlamydomonas reinhardtii SV=2 TOU=Cre") chlamyProteome

//let efficMap = Classification.createDigestionEfficiencyMapFromFasta chlamyProteome



open FSharpAux.IO
open FSharp.Stats
open BioFSharp.Elements



let getData path = 
    FileIO.readFile  path 
    |> Seq.map (fun line -> 
                    let comment = line |> String.split '#'
                    comment.[0].Trim()
                    |> String.split ' ' 
                    |> Array.mapi (fun i x -> 
                        if i = 0 then x else x.Split ':' |> Array.item 1 )
                    |> fun processedLine -> 
                        let rank = processedLine.[0] |> int
                        let query = processedLine.[1] |> int 
                        let feats = 
                            processedLine.[2..processedLine.Length-1] 
                            |> Array.map float
                        rank,(query,comment.[1]),feats
                )
    |> Array.ofSeq
    |> Array.unzip3 
    |> (fun (rank,query,feats) ->
            let normFeats = 
                feats 
                |> JaggedArray.transpose
                /// Hier kommt dann ein Vektor mit länge 45 == anzahl der Features raus. 
                |> Array.map (fun x -> 
                                /// Diesen Mean speichern
                                let mean = FSharp.Stats.Seq.mean x
                                /// Diese Varianz speichern
                                let var = FSharp.Stats.Seq.stDev x 
                                /// Hier erfolgt die Normierung, heißt du musst in deinem Vektor n tuple (mean,varianz) speichern um neue daten zu normieren
                                var,mean
                                )
            normFeats
            )

let normChlamy = getData @"C:\Users\Kevin\Documents\Downloads\unique_allproteins_chlamy_withoutMiss_withInSilico_12032018.dat"
let normYeast = getData @"C:\Users\Kevin\Documents\Downloads\unique_allproteins_yeast_withoutMiss_withInSilico_12032018.dat"

//======================================= for timo

//wenn die function unten nciht geht, hiermit gehts auf jeden fall

let protId = testProt.Header
let gigestionEfficiencyMap = Classification.createDigestionEfficiencyMapFromFasta (seq { yield testProt})
let digested =
    Classification.digestTryptic testProt.Sequence
    |> Seq.map (fun x -> BioArray.toString x)
    |> List.ofSeq

let candidatePeptides = 
    digested
    |> Seq.filter (fun p -> distinctPeptides.Contains(p))
    |> Seq.map (fun p -> Classification.getPeptideFeatures gigestionEfficiencyMap protId (BioArray.ofAminoAcidSymbolString p))
    |> Array.ofSeq
    |> Array.choose id

candidatePeptides
|> Array.map (fun x -> {x with Data= (Array.map2 (fun d (stDev,mean) -> if nan.Equals((d-mean)/stDev) then 0. else (d-mean)/stDev) ) x.Data normChlamy })
|> Prediction.scoreBy (Prediction.Model.Plant)
|> Array.map (fun x -> x.Sequence,x.PredictionScore)
|> Array.sortByDescending (fun (_,s) -> s)
|> fun x -> let max = Array.maxBy (fun (_,s) -> s) x |> snd
            x |> Array.map (fun (sequence,score) -> if score >= 0. then (sequence,(score/max)) else (sequence,0.))
|> Array.mapi (fun i (p,x) -> printfn "%40s\t%.2f" p x) 


let scoreFastasAgainstProteome (proteome:FastaItem<'a> list) (proteinsOfInterest: FastaItem<'a> list)  =

    printfn "distinct"
    let distinctPeptides = Classification.getDistinctTrypticPeptidesFromFasta proteome
    //printfn "%A" distinctPeptides
    printfn "effic map"
    let digestionEfficiencyMap = Classification.createDigestionEfficiencyMapFromFasta proteinsOfInterest
    //printfn "%A" digestionEfficiencyMap
    proteinsOfInterest
    |> List.map (fun protein ->
                                let protId = protein.Header
                                printfn "digesting"
                                let digested =
                                    Classification.digestTryptic protein.Sequence
                                    |> Seq.map (fun x -> BioArray.toString x)
                                    |> Seq.filter (fun peptide -> distinctPeptides.Contains peptide)
                                    |> List.ofSeq
                                printfn "candidates"
                                let candidatePeptides = 
                                    digested
                                    |> Seq.filter (fun p -> distinctPeptides.Contains(p))
                                    |> Seq.map (fun p -> Classification.getPeptideFeatures digestionEfficiencyMap protId (BioArray.ofAminoAcidSymbolString p))
                                    |> Array.ofSeq
                                    |> Array.choose id
                                printfn "scoring"
                                candidatePeptides
                                |> Array.map (fun x -> {x with Data= (Array.map2 (fun d (stDev,mean) -> if nan.Equals((d-mean)/stDev) then 0. else (d-mean)/stDev) ) x.Data normChlamy })
                                |> Prediction.scoreBy (Prediction.Model.Custom @"C:\Users\Kevin\source\repos\CSBiology\BioFSharp\src\BioFSharp.ML\Resources\Chlamy5Times128.model")
                                |> Array.sortByDescending (fun (x) -> x.PredictionScore)
                                |> fun x -> let max = (Array.maxBy (fun (x) -> x.PredictionScore) x).PredictionScore 
                                            x |> Array.map (fun (x) -> if x.PredictionScore >= 0. then {x with PredictionScore = (x.PredictionScore/max)} else {x with PredictionScore = 0.0})
        )
    
scoreFastasAgainstProteome chlamyProteome [testProt]

1+1