namespace BioFSharp.Stats

open System
open System.Collections.Generic
open FSharpAux

module OntologyEnrichment =

    /// Represents an item in an ontology set
    type OntologyItem<'a> = {
        Id               : string
        OntologyTerm     : string
        GroupIndex       : int
        Item             : 'a
        }


    /// Creates an item in an ontology set
    let createOntologyItem id ontologyTerm groupIndex item =
        {Id = id; OntologyTerm = ontologyTerm; GroupIndex = groupIndex; Item = item}

    /// Represents a gene set enrichment result 
    type GseaResult<'a> = {
        OntologyTerm     : string
        ItemsInBin       : seq<OntologyItem<'a>>
        NumberOfDEsInBin : int
        NumberInBin      : int    
        TotalNumberOfDE  : int
        TotalUnivers     : int
        PValue           : float
        }

    /// Creates a gene set enrichment result 
    let createGseaResult ontologyTerm desInBin numberOfDEsInBin numberInBin totalNumberOfDE totalUnivers pValue = 
        {OntologyTerm = ontologyTerm;ItemsInBin = desInBin; NumberOfDEsInBin = numberOfDEsInBin; 
            NumberInBin = numberInBin; TotalNumberOfDE = totalNumberOfDE; TotalUnivers = totalUnivers; PValue = pValue}

    ///Splits an OntologyEntry with seperator concatenated TermIds
    let splitMultipleAnnotationsBy (separator:char) (item:OntologyItem<'A>) =
        let annotations = item.OntologyTerm.Split(separator)
        annotations
        |> Seq.map (fun ot -> {item with OntologyTerm = ot})

    /// Splits MapMan OntologyEntries with seperator concatenated TermIds
    /// Attention: Also parses string to int to get rid of 0 - terms
    let splitMapManOntologyItemsBy (separator:char) (data:seq<OntologyItem<'a>>) =
        let splitTerm (termId:string) (separator:char) =
            termId.Split(separator) 
            |> Array.map (fun sTerm -> 
                let splited = sTerm.Split('.')
                let toInt = splited |> Seq.map (fun v -> Int32.Parse(v).ToString())                                                                            
                toInt  |> String.concat "." 
                         )
        data
        |> Seq.collect (fun oi -> 
            splitTerm oi.OntologyTerm separator
            |> Seq.map (fun sTerm -> createOntologyItem oi.Id sTerm oi.GroupIndex oi.Item)
                        )


    /// Extends leaf OntologyEntries to their full tree
    let expandOntologyTree (data:seq<OntologyItem<'a>>) =
        data
        |> Seq.collect (fun oi -> 
            let expandenTermIds = oi.OntologyTerm.Split('.') |> Array.scanReduce (fun acc elem -> acc + "." + elem)
            expandenTermIds |> Seq.map (fun sTerm -> createOntologyItem oi.Id sTerm oi.GroupIndex oi.Item) 
                       )


    // ###########################################################################################################
    // the hypergeometric distribution is a discrete probability distribution that describes the probability of 
    //   k successes in
    //   n draws from a finite 
    //   x population of size containing
    //   m successes without replacement (successes states)
    /// Calculates p value based on hypergeometric distribution (pValue <= k)
    let CalcHyperGeoPvalue numberOfDEsInBin numberInBin totalUnivers totalNumberOfDE (splitPvalueThreshold:int) =
        if (numberOfDEsInBin > 1) then
            let hp = FSharp.Stats.Distributions.Discrete.hypergeometric totalUnivers totalNumberOfDE numberInBin            
            if numberInBin > splitPvalueThreshold then                                
                // Calculate normal pValue
                1. -  hp.CDF (float (numberOfDEsInBin + 1)) 
            else
                // Calculate split pValue
                0.5 * ((1. -  hp.CDF(float(numberOfDEsInBin + 1)) ) + ( (1. -  hp.CDF(float(numberOfDEsInBin))) ) )
        else
                nan
        
       
    // #######################################################    
    //  functional term enrichment is calculated according to following publication
    //  http://bioinformatics.oxfordjournals.org/cgi/content/abstract/23/4/401
    //  also includes mid-pValues
    /// Calculates functional term enrichment
    let CalcSimpleOverEnrichment (deGroupIndex:int) (splitPvalueThreshold:option<int>) (data:seq<OntologyItem<'a>>) =
        let _splitPvalueThreshold   = defaultArg splitPvalueThreshold 5           
        
        let totalUnivers    = data |> Seq.length
        let totalNumberOfDE = data |> Seq.filter (fun oi -> oi.GroupIndex = deGroupIndex) |> Seq.length
        
        // returns (DE count, all count)
        let countDE (subSet:seq<OntologyItem<'a>>) =             
            let countMap = 
                subSet 
                |> Seq.countBy (fun oi -> if oi.GroupIndex = deGroupIndex then true else false )
                |> Map.ofSeq
            (countMap.TryFindDefault 0 true,(countMap.TryFindDefault 0 true) + (countMap.TryFindDefault 0 false))
        
        data
        |> Seq.groupBy ( fun oi -> oi.OntologyTerm)
        |> Seq.map (fun (oTerm,values) -> 
            let numberOfDEsInBin,numberInBin = countDE values
            let pValue = CalcHyperGeoPvalue numberOfDEsInBin numberInBin totalUnivers totalNumberOfDE _splitPvalueThreshold
            createGseaResult oTerm values numberOfDEsInBin numberInBin totalNumberOfDE totalUnivers pValue)


    // #######################################################    
    //  functional term enrichment is calculated according to following publication
    //  http://bioinformatics.oxfordjournals.org/cgi/content/abstract/23/4/401
    //  also includes mid-pValues
    /// Calculates functional term enrichment
    let CalcOverEnrichment (deGroupIndex:int) (splitPvalueThreshold:option<int>) (minNumberInTerm:option<int>) (data:seq<OntologyItem<'a>>) =
        let _splitPvalueThreshold   = defaultArg splitPvalueThreshold 5
        let _minNumberInTerm        = defaultArg minNumberInTerm 2
        
        // Distinct by term and gene name
        // Has to be done by an ouside function
        //let distinctData    = data |> Seq.distinctBy (fun o -> o.displayID)                
        let gData           = data |> Seq.groupBy ( fun o -> o.OntologyTerm)
        // reduce to terms at least annotated with 2 items
        let fData = gData |> Seq.filter ( fun (key:string,values:seq<OntologyItem<'a>>) -> Seq.length(values) >= _minNumberInTerm)
        let groupCount = fData |> Seq.collect (fun (key:string,values:seq<OntologyItem<'a>>) -> values ) |> Seq.countBy (fun o -> o.GroupIndex)
        
        let totalUnivers    = groupCount |> Seq.fold (fun  (acc:int) (index:int,count:int) -> acc + count) 0
        let totalNumberOfDE = 
            let tmp = groupCount |> Seq.tryFind (fun (key,v) -> key = deGroupIndex)
            if tmp.IsNone then 
                raise (System.ArgumentException("DE group index does not exists in ontology entry"))
            else
                snd(tmp.Value)
        
        // returns (DE count, all count)
        let countDE (subSet:seq<OntologyItem<'a>>) =             
            let countMap = 
                subSet 
                |> Seq.countBy (fun (oi) -> if oi.GroupIndex = deGroupIndex then true else false )
                |> Map.ofSeq
            (countMap.TryFindDefault 0 true,(countMap.TryFindDefault 0 true) + (countMap.TryFindDefault 0 false))
        
        fData
        |> Seq.map (fun (oTerm,values) -> 
            let numberOfDEsInBin,numberInBin = countDE values
            let pValue = CalcHyperGeoPvalue numberOfDEsInBin numberInBin totalUnivers totalNumberOfDE _splitPvalueThreshold
            createGseaResult oTerm values numberOfDEsInBin numberInBin totalNumberOfDE totalUnivers pValue)


