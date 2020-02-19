namespace BioFSharp.Stats

open BioFSharp.Stats
open FSharp.Stats
open OntologyEnrichment

///
module Sailent =
    
    type SailentCharacterization = {
        OntologyTerm : string
        PValue : float
        BinSize: int
        WeightSum: float
    }

    let private createSailentCharacterization ontTerm pVal binSize wS = {OntologyTerm = ontTerm; PValue = pVal; BinSize = binSize; WeightSum=wS}
        

    type SailentResult<'a> = {
        RawData:                OntologyItem<'a> array
        AbsoluteDescriptor:     SailentCharacterization list
        PositiveDescriptor:     SailentCharacterization list
        NegativeDescriptor:     SailentCharacterization list
        BootstrapIterations:    int
    }

    let private createSailentResult raw abs pos neg iter = {RawData=raw; AbsoluteDescriptor=abs; PositiveDescriptor=pos; NegativeDescriptor=neg; BootstrapIterations=iter}

    let private getDistinctGroups (cons:OntologyItem<float> array) =
        [for ann in cons do yield ann.OntologyTerm]
        |> List.distinct

    //create distribution of iter weight sums for a bin of size binSize 
    let private bootstrapBin (binSize: int) (weightArray:float[]) (iter: int) =
        let steps = iter / 10
        let startTime = System.DateTime.Now

        let rec sumRandomEntriesBy k sum =
            if k < binSize then
                sumRandomEntriesBy (k+1) (sum + (weightArray.[Random.rndgen.NextInt(weightArray.Length)]))
            else 
                sum

        let rec loop currentIter resultList =
            if currentIter < iter then
                let tmp = sumRandomEntriesBy 0 0.
                loop (currentIter+1) (tmp::resultList)
            else
                resultList |> Array.ofList

        loop 1 []

    let private getBins (term:string) (cons:OntologyItem<float> array) =
        [for ann in cons do 
            if ann.OntologyTerm = term then yield ann
        ]
    
    let private getNegativeBins (term:string) (cons:OntologyItem<float> array) =
        [for ann in cons do 
            if ann.OntologyTerm = term && ann.Item<0. then yield ann
        ]
    
    let private getPositiveBins (term:string) (cons:OntologyItem<float> array) =
        [for ann in cons do 
            if ann.OntologyTerm = term && ann.Item>0. then yield ann
        ]

    let private getEmpiricalPvalue (testDistributions: Map<int,Map<float,int>>) (weightSum:float) (binSize:int) =
        match Map.tryFind binSize testDistributions with
        |Some dist ->   let testDist = dist
                        float (testDist |> Map.fold (fun acc key value -> if abs key > abs weightSum then acc + value else acc) 0) / (float (testDist |> Map.fold (fun acc key value -> acc + value) 0))
        |_ -> 10000000.

    let private assignPValues (testDistributions:Map<int,Map<float,int>>) (testTargets:(string*int*float)list)=
        testTargets 
        |> List.map (fun (name,binSize,weightSum) -> createSailentCharacterization name (getEmpiricalPvalue testDistributions weightSum binSize) binSize weightSum)
    
    ///utility function to prepare a dataset column for SAILENT characterization. The ontology map can be created by using the BioFSharp.BioDB module. 
    ///
    ///identifiers: a string array containing the annotations of the data at the same index, used as lookup in the ontology map. 
    ///rawData: feature array of interest, must be same length as annotations.
    let prepareDataColumn (ontologyMap:Map<string,(string*string) [] >) (identifiers: string []) (rawData:float []) =

        if rawData.Length <> identifiers.Length then
            failwithf "data column and identifiers dont have the same length (%i vs %i)" rawData.Length identifiers.Length
        else
            let annotatedIds =
                identifiers
                |> Array.map (fun id -> match Map.tryFind id ontologyMap with
                                        |Some ann -> ann |> Array.map snd |> Array.ofSeq
                                        |_ -> [|"35.2"|]
                                        )
            rawData
            |> Array.mapi (fun i x ->  annotatedIds.[i] 
                                       |> Array.map (fun ann -> identifiers.[i],ann,0,x))
            |> Array.concat
                                                          
            |> Array.map (fun (identifier,annotation,indx,value) -> createOntologyItem identifier annotation indx value)

    ///utility function to prepare a dataset (in column major form) for SAILENT characterization. The ontology map can be created by using the BioFSharp.BioDB module.
    ///identifiers: a string array containing the annotations of the data at the same index, used as lookup in the ontology map. 
    ///rawData: feature matrix of interest, columns must have same length as identifiers
    let prepareDataset (ontologyMap:Map<string,(string*string) [] >) (identifiers: string []) (rawDataset:float [] []) =
        rawDataset
        |> Array.map (prepareDataColumn ontologyMap identifiers)

    ///Compute SAILENT (Surprisal AnalysIs EmpiricaL pErmutatioN Test) for the given annotated dataset. This empirical test was
    ///initially designed for the biological application of Surprisal Analysis to test the weight distribution of a given bin of annotations is significantly different than a random distribution 
    ///of the same size given the whole dataset, but it should be applicable to similar types of datasets.
    ///
    ///Input: 
    ///
    ///- verbose: if true, bootstrap iterations and runtime for bootstrapping is printed
    ///
    ///- bootstrapIterations: the amount of distributions to sample from the whole dataset to create test distributions for each binsize present in the data
    ///
    ///- data: annotated dataset (containing ontology items with the associated feature)
    ///
    ///a SAILENT test returns 3 descriptors for the input data:
    ///Absolute descriptor: test distributions and tests are performed on the absolute values of the dataset
    ///Negative descriptor: test distributions and tests are performed on the negative values of the dataset only
    ///Absolute descriptor: test distributions and tests are performed on the positive values of the dataset only
    let compute (verbose:bool) (bootstrapIterations:int) (data: OntologyItem<float> array) =

        if verbose then printfn "starting SAILENT characterization"

        // get distinct ontology terms in the dataset
        let distinctGroups = getDistinctGroups data

        // allocate test targets from the dataset
        let absoluteTestTargets = 
            distinctGroups
            |> List.map (fun (termName)
                            ->  let tmp = getBins termName data
                                termName,tmp.Length,tmp |> List.sumBy (fun x -> abs x.Item))
            |> List.filter (fun (termName,binSize,weightSum) -> binSize>0)

        let positiveTestTargets =
            distinctGroups
            |> List.map (fun (termName)
                            ->  let tmp = getPositiveBins termName data
                                termName,tmp.Length,tmp |> List.sumBy (fun x -> x.Item))
            |> List.filter (fun (termName,binSize,weightSum) -> binSize>0)

        let negativeTestTargets =
            distinctGroups
            |> List.map (fun (termName)
                            ->  let tmp = getNegativeBins termName data
                                termName,tmp.Length,tmp |> List.sumBy (fun x -> x.Item))
            |> List.filter (fun (termName,binSize,weightSum) -> binSize>0)

        let absoluteBinsizes = absoluteTestTargets |> List.map (fun (_,binSize,_) -> binSize) |> List.distinct
        let positiveBinsizes = positiveTestTargets |> List.map (fun (_,binSize,_) -> binSize) |> List.distinct
        let negativeBinsizes = negativeTestTargets |> List.map (fun (_,binSize,_) -> binSize) |> List.distinct

        let weightArr =     data        |> Array.map (fun ann -> ann.Item)
        let absWeightArr =  weightArr   |> Array.map abs
        let posWeightArr =  weightArr   |> Array.filter(fun x -> x>0.)
        let negWeightArr =  weightArr   |> Array.filter(fun x -> x<0.)


        // create bootstrapped test distributions for all test targets
        if verbose then printfn "bootstrapping absolute test distributions for %i bins" absoluteBinsizes.Length
        let absoluteTestDistributions =

            let startTime = System.DateTime.Now

            absoluteBinsizes
            |> List.mapi 
                (fun i binSize ->

                    if verbose && (i % (absoluteBinsizes.Length / 10) = 0 ) then
                        let elapsed = System.DateTime.Now.Subtract(startTime)
                        printfn "[%i/%i] bins @ %imin %is" i absoluteBinsizes.Length elapsed.Minutes elapsed.Seconds

                    let tmp = bootstrapBin binSize absWeightArr bootstrapIterations
                    (binSize,Distributions.Frequency.create (Distributions.Bandwidth.nrd0 tmp) tmp)
                )
            |> Map.ofList

        if verbose then printfn "bootstrapping positive test distributions for %i bins" positiveBinsizes.Length
        let positiveTestDistributions =

            let startTime = System.DateTime.Now

            positiveBinsizes
            |> List.mapi 
                (fun i binSize ->

                    if verbose && (i % (positiveBinsizes.Length / 10) = 0 ) then
                        let elapsed = System.DateTime.Now.Subtract(startTime)
                        printfn "[%i/%i] bins @ %imin %is" i positiveBinsizes.Length elapsed.Minutes elapsed.Seconds

                    let tmp = bootstrapBin binSize posWeightArr bootstrapIterations
                    (binSize,Distributions.Frequency.create (Distributions.Bandwidth.nrd0 tmp) tmp)
                )
            |> Map.ofList
            
        if verbose then printfn "bootstrapping negative test distributions for %i bins" negativeBinsizes.Length
        let negativeTestDistributions = 

            let startTime = System.DateTime.Now

            negativeBinsizes
            |> List.mapi 
                (fun i binSize ->

                    if verbose && (i % (negativeBinsizes.Length / 10) = 0 ) then
                        let elapsed = System.DateTime.Now.Subtract(startTime)
                        printfn "[%i/%i] bins @ %imin %is" i negativeBinsizes.Length elapsed.Minutes elapsed.Seconds

                    let tmp = bootstrapBin binSize negWeightArr bootstrapIterations
                    (binSize,Distributions.Frequency.create (Distributions.Bandwidth.nrd0 tmp) tmp)
                )
            |> Map.ofList

        if verbose then printfn "assigning empirical pValues for all bins..."

        //assign Pvalues for all test targets
        let absResults = assignPValues absoluteTestDistributions absoluteTestTargets
        let posResults = assignPValues positiveTestDistributions positiveTestTargets
        let negResults = assignPValues negativeTestDistributions negativeTestTargets

        createSailentResult data absResults posResults negResults bootstrapIterations


    ///Compute SAILENT (Surprisal AnalysIs EmpiricaL pErmutatioN Test) for the given Surprisal Analysis result. This empirical test was
    ///designed for the biological application of Surprisal Analysis to test the weight distribution of a given bin of annotations is significantly different than a random distribution 
    ///of the same size given the whole dataset.
    ///
    ///Input: 
    ///
    ///- verbose: if true, bootstrap iterations and runtime for bootstrapping is printed
    ///
    ///- ontologyMap: maps identifiers of the data to ontology annotations (can be created using the BioFSharp.BioDB module)
    ///
    ///- identifiers: a string array containing the annotations of the data at the same index, used as lookup in the ontology map. 
    ///
    ///- bootstrapIterations: the amount of distributions to sample from the whole dataset to create test distributions for each binsize present in the data
    ///
    ///- saRes: the Surprisal Analysis Result to test
    ///
    ///a SAILENT test returns 3 descriptors for each constraint of the Surprisal Nalysis result:
    ///Absolute descriptor: test distributions and tests are performed on the absolute values of the dataset
    ///Negative descriptor: test distributions and tests are performed on the negative values of the dataset only
    ///Absolute descriptor: test distributions and tests are performed on the positive values of the dataset only

    let computeOfSARes (verbose:bool) (ontologyMap:Map<string,(string*string) [] >) (identifiers: string []) (bootstrapIterations:int) (saRes:FSharp.Stats.ML.SurprisalAnalysis.SAResult) =
        saRes.MolecularPhenotypes
        |> Matrix.toJaggedArray
        // Matrices are sadly row major =(
        |> JaggedArray.transpose
        |> prepareDataset ontologyMap identifiers
        |> Array.mapi 
            (fun i p ->
                if verbose then printfn "Sailent of constraint %i" i
                compute verbose bootstrapIterations p
            ) 
    
    ///Async version of computeOfSARes to use for parallelization (computeOfSAResAsync ( .. ) |> Async.Parallel |> Async.RunSynchronously)
    let computeOfSAResAsync (verbose:bool) (ontologyMap:Map<string,(string*string) [] >) (identifiers: string []) (bootstrapIterations:int) (saRes:FSharp.Stats.ML.SurprisalAnalysis.SAResult) =
        saRes.MolecularPhenotypes
        |> Matrix.toJaggedArray
        // Matrices are sadly row major =(
        |> JaggedArray.transpose
        |> prepareDataset ontologyMap identifiers
        |> Array.mapi 
            (fun i p ->
                async {
                    return compute verbose bootstrapIterations p
                }
            ) 
    
