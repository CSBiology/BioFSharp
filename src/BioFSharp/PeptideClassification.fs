namespace BioFSharp

///Contains functions to classify peptides based on their specificity when mapped to proteins
module PeptideClassification =
    
    open FSharpAux
    open System.Collections.Generic

    ///Direction of strand
    type StrandDirection =
        | Forward = 0
        | Reverse = 1

    /// Contains information about protein used to deduce its relation to other proteins
    type ProteinModelInfo<'id,'chromosomeId,'geneLocus when 'id: comparison and 'chromosomeId: comparison and 'geneLocus: comparison> = {
        Id              : 'id
        ChromosomeId    : 'chromosomeId
        Strand          : StrandDirection
        GeneLocus       : 'geneLocus
        SpliceVariantId : int
        SeqEquivalent   : Set< ProteinModelInfo<'id,'chromosomeId,'geneLocus> >
        Orthologs       : Set< ProteinModelInfo<'id,'chromosomeId,'geneLocus> >
        }

    /// Helper function to create ProteinModelInfo
    let createProteinModelInfo id chromosomeId strand geneLocus spliceVariantId seqEquivalents orthologs = {
        Id               = id
        ChromosomeId     = chromosomeId
        Strand           = strand
        GeneLocus        = geneLocus
        SpliceVariantId  = spliceVariantId
        SeqEquivalent    = Set.ofSeq seqEquivalents
        Orthologs        = Set.ofSeq orthologs
        }       

    /// Contains protein sequence and information about protein used to deduce its relation to other proteins
    type ProteinModel<'id,'chromosomeId,'geneLocus,'sequence when 'id: comparison and 'chromosomeId: comparison and 'geneLocus: comparison and 'sequence: comparison> = {
        ProteinModelInfo : ProteinModelInfo<'id,'chromosomeId,'geneLocus>
        Sequence : 'sequence 
        }

    /// Helper function
    let createProteinModel proteinModelInfo sequence = 
        {ProteinModelInfo=proteinModelInfo;Sequence=sequence}
    
    /// A marker for unambiguity of a peptide in protein inference 
    type PeptideEvidenceClass = 
        | Unknown   = 0
        //Maps to exactly one isoform of one protein
        | C1a       = 1
        //Maps to different isoforms of one protein
        | C1b       = 2
        | C2a       = 3
        | C2b       = 4
        | C3a       = 5
        | C3b       = 6

    /// Creates a lookup data base to assign peptides to the proteins they are contained in
    let createPeptideProteinRelation digest (protModels:seq<ProteinModel<'id,'chromosomeId,'geneLocus,'sequence> option>) =
        let ppRelation = BidirectionalDictionary<'sequence,ProteinModelInfo<'id,'chromosomeId,'geneLocus>>()
        protModels            
        |> Seq.iteri (fun i prot ->                                              
                        // insert peptide-protein relationship
                        // Todo: change type of proteinID in digest
                        match prot with 
                        | Some proteinModel ->
                            digest proteinModel.Sequence
                            |> Seq.iter (fun pepSequence -> ppRelation.Add pepSequence proteinModel.ProteinModelInfo)                
                        | None                   -> ()
                    )
        ppRelation  
   
        
    // One could also sort the fasta according to the locusId and then count splicevariants on the fly, but this procedure is failsafe
    let createLocusSpliceVariantCount (ppRelation: BidirectionalDictionary<'sequence,ProteinModelInfo<'id,'chromosomeId,'geneLocus>>) = 
        let gLocusToSplVarNr = Dictionary<'geneLocus, int>()
        ppRelation.GetArrayOfValues
        |> Array.iter
            (fun x -> 
                match gLocusToSplVarNr.TryGetValue x.GeneLocus with
                | true, count -> 
                    gLocusToSplVarNr.[x.GeneLocus] <- count + 1
                | false, _   -> 
                    gLocusToSplVarNr.Add(x.GeneLocus, 1)
            )     
        gLocusToSplVarNr    

    ///Assigns a PeptideEvidenceClass to the peptide by using the information given through lookup and proteinInfos
    let classify (lookUp:Dictionary<'geneLocus,int>) (peptide,proteinInfos:seq<ProteinModelInfo<_,_,'geneLocus>>) =
    
        let isGeneUnambiguous (pmi:seq< ProteinModelInfo<'id,'chromosomeId,'geneLocus> >) =
            // Consider using c# HashMap
            let tmp =
                pmi 
                |> Seq.map (fun p -> p.ChromosomeId,p.Strand,p.GeneLocus) 
                |> Set.ofSeq
            tmp.Count = 1


        let isProteinSeqUnambiguous (pmi:seq< ProteinModelInfo<'id,'chromosomeId,'geneLocus> >) =
            let allSeqEquivalent = 
                Set.unionMany (pmi |> Seq.map (fun p -> p.SeqEquivalent))
            let all = Set.ofSeq pmi
            if all.Count > 0 then
                all = allSeqEquivalent       
            else
                failwithf "At least one ProteinModelInfo is needed"


        let hasMultipleIsoforms (pmi:seq< ProteinModelInfo<'id,'chromosomeId,'geneLocus> >) =
            // Consider using c# HashMap
            let tmp =
                pmi 
                |> Seq.map (fun p -> p.SpliceVariantId) 
                |> Set.ofSeq
            tmp.Count <> 1


        let isIsoformSubSet (lookUp:Dictionary<'geneLocus,int>) (proteinInfos:seq<ProteinModelInfo<_,_,'geneLocus>>) =
            let protCount = Seq.length proteinInfos
            if protCount > 1 then
                let tmp = Seq.head proteinInfos
                if lookUp.ContainsKey tmp.GeneLocus then
                    protCount < lookUp.[tmp.GeneLocus]
                else
                    failwithf "Protein not in gene locus lookUp"
            else
                false

        // ###### Classify 
        // Maps to only one protein model (unambiguous)
        match  isGeneUnambiguous proteinInfos with
        | false -> 
            // Only genes with same protein sequence
            match isProteinSeqUnambiguous proteinInfos with
            | true  -> PeptideEvidenceClass.C3a, peptide
            | false -> PeptideEvidenceClass.C3b, peptide
        | true ->        
            // Maps to multiple isoforms
            match hasMultipleIsoforms proteinInfos with
            | false -> PeptideEvidenceClass.C1a, peptide        
            | true  -> 
                // If we are here all have the same locus -> isGeneUnambiguous = true
                // Subsets different isoforms
                match isIsoformSubSet lookUp proteinInfos with
                | false -> PeptideEvidenceClass.C2b, peptide
                | true  -> 
                    // different splice variance with the same sequence
                    match isProteinSeqUnambiguous proteinInfos with
                    | true  -> PeptideEvidenceClass.C1b, peptide
                    | false -> PeptideEvidenceClass.C2a, peptide

