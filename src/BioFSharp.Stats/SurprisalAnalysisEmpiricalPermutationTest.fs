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

    let getDistinctGroups (cons:OntologyItem<float> array) =
        [for ann in cons do yield ann.OntologyTerm]
        |> List.distinct
    
    //create distribution of iter weight sums for a bin of size binSize 
    let private bootstrapBin (binSize: int) (weightArray:float[]) (iter: int) =
        let rec loop currentIter resultList =
            if currentIter < iter then
                let tmp = Array.shuffleFisherYates weightArray
                loop (currentIter+1) ((Array.sum (tmp.[0..binSize-1]))::resultList)
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
    let prepareDataColumn (ontologyMap:Map<string,(string*string)list>) (identifiers: string []) (rawData:float []) =

        if rawData.Length <> identifiers.Length then
            failwithf "data column and identifiers dont have the same length (%i vs %i)" rawData.Length identifiers.Length
        else
            let annotatedIds =
                identifiers
                |> Array.map (fun id -> match Map.tryFind id ontologyMap with
                                        |Some ann -> ann |> List.map snd |> Array.ofSeq
                                        |_ -> [|"35.2"|]
                                        )
            rawData
            |> Array.mapi (fun i x ->  annotatedIds.[i] 
                                       |> Array.map (fun ann -> identifiers.[i],ann,0,x))
            |> Array.concat
                                                          
            |> Array.map (fun (identifier,annotation,indx,value) -> createOntologyItem identifier annotation indx value)

    ///utility function to prepare a dataset (in column major form) for SAILENT characterization. The ontology map can be created by using the BioFSharp.BioDB module.
    let prepareDataset (ontologyMap:Map<string,(string*string)list>) (identifiers: string []) (rawDataset:float [] []) =
        rawDataset
        |> Array.map (prepareDataColumn ontologyMap identifiers)

    let compute (bootstrapIterations:int) (data: OntologyItem<float> array) =

        printfn "starting SAILENT characterization"

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

        let absoluteBinsizes =  [for (_,binSize,_) in absoluteTestTargets do yield binSize]
        let positiveBinsizes =  [for (_,binSize,_) in positiveTestTargets do yield binSize]
        let negativeBinsizes =  [for (_,binSize,_) in negativeTestTargets do yield binSize]

        let weightArr =     [|for ann in data do yield ann.Item|]
        let posWeightArr =  [|for ann in data do yield ann.Item|] |> Array.filter(fun x -> x>0.)
        let negWeightArr =  [|for ann in data do yield ann.Item|] |> Array.filter(fun x -> x<0.)


        // create bootstrapped test distributions for all test targets
        printfn "bootstrapping absolute test distributions..."
        let absoluteTestDistributions =
            [|  
                for binSize in absoluteBinsizes do
                let tmp = bootstrapBin binSize weightArr bootstrapIterations
                yield (binSize,Distributions.Frequency.create (Distributions.Bandwidth.nrd0 tmp) tmp)
            |]
            |> Map.ofArray

        printfn "bootstrapping positive test distributions..."
        let positiveTestDistributions =
            [|  
                for binSize in positiveBinsizes do
                let tmp = bootstrapBin binSize posWeightArr bootstrapIterations
                yield (binSize,Distributions.Frequency.create (Distributions.Bandwidth.nrd0 tmp) tmp)
            |]
            |> Map.ofArray
        
        printfn "bootstrapping negative test distributions..."
        let negativeTestDistributions = 
            [|  
                for binSize in negativeBinsizes do
                let tmp = bootstrapBin binSize negWeightArr bootstrapIterations
                yield (binSize,Distributions.Frequency.create (Distributions.Bandwidth.nrd0 tmp) tmp)
            |]
            |> Map.ofArray

        //assign Pvalues for all test targets
        let absResults = assignPValues absoluteTestDistributions absoluteTestTargets
        let posResults = assignPValues positiveTestDistributions positiveTestTargets
        let negResults = assignPValues negativeTestDistributions negativeTestTargets

        createSailentResult data absResults posResults negResults bootstrapIterations
