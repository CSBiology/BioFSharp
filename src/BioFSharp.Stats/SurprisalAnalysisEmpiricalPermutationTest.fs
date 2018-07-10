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

    let getEmpiricalPvalue (testDistributions: Map<int,Map<float,int>>) (weightSum:float) (binSize:int) =
        match Map.tryFind binSize testDistributions with
        |Some dist ->   let testDist = dist
                        float (testDist |> Map.fold (fun acc key value -> if abs key > abs weightSum then acc + value else acc) 0) / (float (testDist |> Map.fold (fun acc key value -> acc + value) 0))
        |_ -> 10000000.

    let assignPValues (testDistributions:Map<int,Map<float,int>>) (testTargets:(string*int*float)list)=
        testTargets 
        |> List.map (fun (name,binSize,weightSum) -> createSailentCharacterization name (getEmpiricalPvalue testDistributions weightSum binSize) binSize weightSum)

    let compute (bootstrapIterations:int) (cons: OntologyItem<float> array) =

        printfn "starting SAILENT characterization"

        // get distinct ontology terms in the dataset
        let distinctGroups = getDistinctGroups cons

        // allocate test targets from the dataset
        let absoluteTestTargets = 
            distinctGroups
            |> List.map (fun (termName)
                            ->  let tmp = getBins termName cons
                                termName,tmp.Length,tmp |> List.sumBy (fun x -> abs x.Item))
            |> List.filter (fun (termName,binSize,weightSum) -> binSize>0)

        let positiveTestTargets =
            distinctGroups
            |> List.map (fun (termName)
                            ->  let tmp = getPositiveBins termName cons
                                termName,tmp.Length,tmp |> List.sumBy (fun x -> x.Item))
            |> List.filter (fun (termName,binSize,weightSum) -> binSize>0)

        let negativeTestTargets =
            distinctGroups
            |> List.map (fun (termName)
                            ->  let tmp = getNegativeBins termName cons
                                termName,tmp.Length,tmp |> List.sumBy (fun x -> x.Item))
            |> List.filter (fun (termName,binSize,weightSum) -> binSize>0)

        let absoluteBinsizes =  [for (_,binSize,_) in absoluteTestTargets do yield binSize]
        let positiveBinsizes =  [for (_,binSize,_) in positiveTestTargets do yield binSize]
        let negativeBinsizes =  [for (_,binSize,_) in negativeTestTargets do yield binSize]

        let weightArr =     [|for ann in cons do yield ann.Item|]
        let posWeightArr =  [|for ann in cons do yield ann.Item|] |> Array.filter(fun x -> x>0.)
        let negWeightArr =  [|for ann in cons do yield ann.Item|] |> Array.filter(fun x -> x<0.)


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

        createSailentResult cons absResults posResults negResults bootstrapIterations