namespace BioFSharp.Mz

open System
open BioFSharp
open BioFSharp.Mz.Peaks
open BioFSharp.Mz.PeakArray
open SearchDB
open Fragmentation

open FSharp.Care

open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra.Double

module AndromedaLike =

    type PeakFamily<'a> = {
        MainPeak       : 'a
        DependentPeaks : 'a list   
        }

    let createPeakFamily mainPeak dependentPeaks = {
        MainPeak       = mainPeak
        DependentPeaks = dependentPeaks  
        }
    ///
    type AndromedaLikeScore = { 
            SpectrumID      : string;
            ModSequenceID   : int
            PepSequenceID   : int;
            GlobalMod       : int;
            IsTarget        : bool;
            StringSequence  : string;
            PrecursorCharge : int;
            PrecursorMZ     : float;
            MeasuredMass    : float;
            TheoMass        : float;            
            PeptideLength   : int;
            AndroScore      : float;
            DeltaAndroScore : float; 
        }

    ///
    let createAndromedaScore spectrumID modSequenceID pepSequenceID globalMod isTarget stringSequence precursorCharge precursorMZ measuredMass theoMass peptideLength androScore deltaAndroScore = 
        { SpectrumID = spectrumID;ModSequenceID=modSequenceID; PepSequenceID = pepSequenceID; GlobalMod = globalMod; IsTarget = isTarget; StringSequence = stringSequence; 
            PrecursorCharge = precursorCharge; PrecursorMZ=precursorMZ;  MeasuredMass = measuredMass; TheoMass = theoMass; PeptideLength = peptideLength; AndroScore = androScore; DeltaAndroScore = deltaAndroScore; }


    ///
    let nrOfBins (scanlimits:float*float)  = 
        let lowerLimit,upperLimit = scanlimits
        ((upperLimit-lowerLimit) / 100. / 2.)  
        |> (*) 2.
        |> ceil
        |> int

    ///
    let spectrumToBinnedArray qMostAbundandPepsMin qMostAbundandPepsMax (scanlimits:float*float) (spectrumData:(float*float) []) = //(scanlimits:float*float) 
        /// TODO cut off specData < lowerlimit und specdata > upperlimit
        if spectrumData.Length = 0 then [||]
        else
        let lowerLimit,upperLimit = scanlimits
        // Number of bins with width of 100 Thomson (th = m/z)
        let nrOfBins = 
            ((upperLimit-lowerLimit) / 100. / 2.)  
            |> (*) 2.
            |> ceil
            |> int
        ///
        let binnedData = 
            let tmp = Array.create nrOfBins []
            for i = 0 to spectrumData.Length-1 do
                let binNumber = 
                    fst spectrumData.[i] / 100.
                    |> int 
                tmp.[binNumber-1] <- spectrumData.[i] :: tmp.[binNumber-1]
            tmp
        ///
        let intensitySortedBins = 
            binnedData
            |> Array.map (fun binL -> 
                            binL
                            |> List.sortByDescending (fun (mzData,intensity) -> intensity)
                            |> (fun x -> if x.Length >= qMostAbundandPepsMax then
                                              List.take qMostAbundandPepsMax x
                                         else List.take x.Length x
                               )
                         ) 
        ///
        let jaggedIntensitySortedBins = 
            [|for q = qMostAbundandPepsMin to qMostAbundandPepsMax do
                yield
                   q, [for l = 0 to nrOfBins-1 do
                        let currentbin = intensitySortedBins.[l]
                    
                        if currentbin.Length > 0 && currentbin.Length >= q then              
                            yield currentbin 
                                  |> List.take q
                                  |> List.sortBy (fun (mzData,intensity) -> mzData)
                             
                        else
                            yield currentbin 
                                  |> List.sortBy (fun (mzData,intensity) -> mzData)     
                    ]
        
            |]
        jaggedIntensitySortedBins
            
    ///
    let ions (massfunction:Formula.Formula -> float) (ionSeries: IonSeries) (aal:AminoAcids.AminoAcid list)  = 
        let rec series aminoList fragMasses acc =
            match aminoList with
            | f::rest    -> 
                            let currentMass = massfunction (AminoAcids.formula f)
                       
                            let mainIonMass     = acc + currentMass 

                            let mainPeak        = 

                                createTag (true,(Main ionSeries)) mainIonMass
                        
                            let lossNH3Ion  = 
                                let mass = (acc + currentMass - (massfunction (AminoAcids.isotopicLabelFunc f Formula.Table.NH3))) 
                                if BioFSharp.Mz.Fragmentation.isAminoLoss f then 
                                    Some (createTag (true,(LossNH3 ionSeries)) mass)
                                else  
                                    Some (createTag (false,(LossNH3 ionSeries)) mass)

                            let lossH2OIon   = 
                                let mass = (acc + currentMass - (massfunction (AminoAcids.isotopicLabelFunc f Formula.Table.H2O))) 
                                if BioFSharp.Mz.Fragmentation.isWaterLoss f then 
                                    Some (createTag (true,(LossH20 ionSeries)) mass)
                                else  
                                    Some (createTag (false,(LossH20 ionSeries)) mass)
                        
                            let lossNeutral = 
                                if BioFSharp.Mz.Fragmentation.isNeutralLoss f then 
                                    match f with 
                                    | AminoAcids.Mod(aa, modiL) ->  
                                        let mass = (acc + currentMass - ( massfunction (AminoAcids.isotopicLabelFunc f (modiL.Head.Modify Formula.emptyFormula)) ) )
                                        Some (createTag (true,(NeutralLoss ionSeries)) mass)
                                    | _ -> None
                                else None
                            let tmp = []
                            let dependentPeaks = 
                                lossNH3Ion::lossH2OIon::lossNeutral::tmp 
                                |> List.filter (fun annotPeak -> annotPeak.IsSome)
                                |> List.map (fun annotPeak -> annotPeak.Value)

                            let peakFam =
                                createPeakFamily mainPeak dependentPeaks
                            series (rest) (peakFam::fragMasses) mainIonMass
        
            | _          -> fragMasses

        ///
        match ionSeries with 
        | IonSeries.B -> 
            series aal [] 0.0 
            |> List.rev
        | IonSeries.Y -> 
            series (aal |> List.rev) [] (massfunction Formula.Table.H2O) 
        | _           -> []

   
    ///
    let predictOf (massfunction:Formula.Formula -> float) (scanlimits:float*float) (aal:list<AminoAcids.AminoAcid>) (maxcharge:float) =
        let lowerLimit,upperLimit = scanlimits

        //a series missing
        let b'bNbH_list = ions massfunction IonSeries.B aal
        let y'yNyH_list = ions massfunction IonSeries.Y aal

        let rec recloop (ions) (charge) (b'bNbH_list:List<PeakFamily<Tag<bool*IonTypes,float>>>) (y'yNyH_list:List<PeakFamily<Tag<bool*IonTypes,float>>>) =
            match b'bNbH_list,y'yNyH_list with

            | (bFamily)::restb,(yFamily)::resty ->  
                ///
                let bMain = 
                    let bMainPeak = 
                        createPeak (Mass.toMZ bFamily.MainPeak.Data charge) 1.
                    createPeakAnnotation (snd bFamily.MainPeak.Meta) bMainPeak
                                                  
                let bDependent =
                    bFamily.DependentPeaks
                        |> List.fold (fun acc dependent -> 
                                        if charge <= 1. || (fst dependent.Meta) = true then
                                            let bPeak = 
                                                createPeak (Mass.toMZ dependent.Data charge) 1.
                                            (createPeakAnnotation (snd dependent.Meta) bPeak) :: acc
                                        else 
                                            acc
                                    ) []
                let bPeakFam = 
                    createPeakFamily bMain bDependent       

                ///    
                let yMain = 
                    let yMainPeak = 
                        createPeak (Mass.toMZ yFamily.MainPeak.Data charge) 1.
                    createPeakAnnotation (snd yFamily.MainPeak.Meta) yMainPeak
                                                  
                let yDependent =
                    yFamily.DependentPeaks
                        |> List.fold (fun acc dependent -> 
                                        if charge <= 1. || (fst dependent.Meta) = true then
                                            let yPeak = 
                                                createPeak (Mass.toMZ dependent.Data charge) 1.
                                            (createPeakAnnotation (snd dependent.Meta) yPeak) :: acc
                                        else
                                            acc 
                                    ) []
                let yPeakFam = 
                    createPeakFamily yMain yDependent                                               
            
                ///
                recloop (bPeakFam::yPeakFam::ions) charge restb resty

            | [],_ -> ions
            | _,[] -> ions
        
        // if precursor charge is greater than 1 include Ion Series of charge 2   
        if maxcharge > 1. then         
            [| for z = 1. to 2. do
                yield! recloop [] z b'bNbH_list y'yNyH_list |] 
            |> Array.filter (fun peak -> peak.MainPeak.Data.Mz >= lowerLimit && peak.MainPeak.Data.Mz <= upperLimit)
            |> Array.sortBy (fun peak -> peak.MainPeak.Data.Mz)
        else 
            recloop [] 1. b'bNbH_list y'yNyH_list
            |> List.toArray
            |> Array.filter (fun peak -> peak.MainPeak.Data.Mz >= lowerLimit && peak.MainPeak.Data.Mz <= upperLimit)
            |> Array.sortBy (fun peak -> peak.MainPeak.Data.Mz)

    ///
    let findBinIdx mz =
        (mz / 100. |> int) - 1

    let hasMatchingPeakMZTol (mzMatchingTolerance: float) (tarmz: float) (observBinnedSpect: ((float*float) list) list)  =
        ///
        let targetBin = findBinIdx tarmz
        ///
        match List.tryFind ( fun (mz,int) -> ( (mz-tarmz) |> abs ) <= mzMatchingTolerance) observBinnedSpect.[targetBin]  with
        | Some _    -> true
        | None      -> false

    let hasMatchingPeakPPM (mzMatchingTolerancePPM: float)  (tarmz: float) (observBinnedSpect: ((float*float) list) list)  =
        ///
        let targetBin = findBinIdx tarmz
        if targetBin < 0 then false 
        else
        ///
        match List.tryFind ( fun (mz,int) -> ( (mz-tarmz) |> abs ) <= Mass.deltaMassByPpm mzMatchingTolerancePPM tarmz) observBinnedSpect.[targetBin]  with
        | Some _    -> true
        | None      -> false


    let countMatches (scanlimits:float*float) (mzMatchingTolerance: float) (theoSpect: PeakFamily<PeakAnnotation> []) (observBinnedSpect:int* ((float*float) list) list ) =    
        ///
        let lowerLimit,upperLimit = scanlimits
        let (q, binnedSpec) = observBinnedSpect

        /// declare variables
        let mutable nWithOutNeutralAcc  = 0
        let mutable nWithNeutralAcc     = 0
        let mutable kWithOutNeutralAcc  = 0
        let mutable kWithNeutralAcc     = 0

        let rec findMatches (mzMatchingTolerance: float) (theoSpect: PeakFamily<PeakAnnotation> []) (observBinnedSpect: ((float*float) list) list ) idx nWithOutNeutral nWithNeutral kWithOutNeutral kWithNeutral =
            if idx = theoSpect.Length then
                nWithOutNeutral, nWithNeutral, kWithOutNeutral, kWithNeutral
            else
                let currentPeakFam = theoSpect.[idx]
                //
                nWithOutNeutralAcc  <- 0
                nWithNeutralAcc     <- 0
                //
                kWithOutNeutralAcc  <- 0
                kWithNeutralAcc     <- 0


                if hasMatchingPeakPPM mzMatchingTolerance currentPeakFam.MainPeak.Data.Mz observBinnedSpect = true then
                    // raise ns
                    nWithOutNeutralAcc  <- 1
                    nWithNeutralAcc     <- 1     
                    // raise ks                          
                    kWithOutNeutralAcc  <- 1
                    kWithNeutralAcc     <- 1
                
                    currentPeakFam.DependentPeaks 
                    |> List.iter ( fun dependentAnnotPeak -> 

                                     if hasMatchingPeakPPM mzMatchingTolerance dependentAnnotPeak.Data.Mz observBinnedSpect = true then
                                     //Successful match: raise n and k
                                        match dependentAnnotPeak.Meta with
                                        //If NeutralLoss occurs only raise the neutralLossAccs 
                                        | IonTypes.NeutralLoss _ ->
                                            nWithNeutralAcc     <- nWithNeutralAcc    + 1                               
                                            kWithNeutralAcc     <- kWithNeutralAcc    + 1

                                        | _                      ->
                                            nWithOutNeutralAcc  <- nWithOutNeutralAcc + 1
                                            nWithNeutralAcc     <- nWithNeutralAcc    + 1     
                                                                  
                                            kWithOutNeutralAcc  <- kWithOutNeutralAcc + 1
                                            kWithNeutralAcc     <- kWithNeutralAcc    + 1                                          
                                     else 

                                     //Unsuccessful match: raise n
                                        match dependentAnnotPeak.Meta with 
                                        |  IonTypes.NeutralLoss _ ->
                                            nWithNeutralAcc     <- nWithNeutralAcc    + 1

                                        | _                       ->      
                                            nWithOutNeutralAcc  <- nWithOutNeutralAcc + 1
                                            nWithNeutralAcc     <- nWithNeutralAcc    + 1                              
                                  )  

                    findMatches mzMatchingTolerance theoSpect observBinnedSpect (idx+1) (nWithOutNeutral + nWithOutNeutralAcc) (nWithNeutral + nWithNeutralAcc) (kWithOutNeutral + kWithOutNeutralAcc) (kWithNeutralAcc + kWithNeutral)
            
                else 
                    // No matching Peak. Raise boths ks and both idx.
                    findMatches mzMatchingTolerance theoSpect observBinnedSpect (idx+1) (nWithOutNeutral+1) (nWithNeutral+1) kWithOutNeutral kWithNeutral

        let (nWithOutNeutral, nWithNeutral, kWithOutNeutral,kWithNeutral)  = findMatches mzMatchingTolerance theoSpect binnedSpec 0 0 0 0 0
        (nWithOutNeutral, nWithNeutral, kWithOutNeutral, kWithNeutral) 

    ///
    let gammLn x = MathNet.Numerics.SpecialFunctions.GammaLn(x)

    ///
    let lnProb (n:float) (k:float) (lnp:float) (lnq:float)  =
        let score = 
            -k * lnp - (n - k) * lnq -  gammLn (n + 1.) + gammLn (k + 1.) + gammLn (n - k + 1.); 
        score

    ///
    let scoreFuncImpl n k topx =     
        let topx' = topx |> float
        let p1  = Math.Min(topx'/100.,0.5)
        let lnp = Math.Log(p1)
        let lnq = Math.log2(1. - p1)
        let mutable p = 0.
        for i = k to n do
            let n' = n |> float
            let k' = i |> float
            p <- p + Math.Exp( - lnProb n'  k' lnp lnq )
        p <- -Math.Log(p)
        10. * p / Math.Log(10.)

    let scoreTheoVsRecordedSpec scanlimits matchingTolPPM theoSpec binnedRecSpec= 
        binnedRecSpec
        |> Array.map (fun (q,l) -> q, countMatches scanlimits matchingTolPPM theoSpec (q,l))
        |> Array.map (fun (q,(nWo,nWi,kWo,kWi)) -> 
                                let score1 = scoreFuncImpl nWo kWo q
                                let score2 = scoreFuncImpl nWi kWi q 
                                if score1 > score2 then score1 else score2
                        )
        |> Array.max 

    let calcDeltaAndorScore (sourceList:AndromedaLikeScore list) =
        match sourceList with
        | h1::rest -> 
            sourceList
            |> List.map
                (fun sls ->
                    let deltaAndroScore = (h1.AndroScore - sls.AndroScore) / h1.AndroScore
                    { sls with DeltaAndroScore = deltaAndroScore } )      
        | []       -> []

    ///
    let calcAndromedaLikeScoresRevDecoy (massfunction:Formula.Formula -> float) (qMinAndMax) (scanlimits) matchingTolPPM (spectrum:PeakArray<_>) chargeState isolationWindowTargetMz (possiblePeptideInfos:list<LookUpResult<AminoAcids.AminoAcid>>) spectrumID =
        //
        let recordedSpec = 
            spectrum |> Array.map (fun peak -> peak.Mz,peak.Intensity)
        // 
        let binnedRecSpec = 
            spectrumToBinnedArray (fst qMinAndMax) (snd qMinAndMax) (scanlimits:float*float) (recordedSpec:(float*float) [])
        //
        let fCharge = float chargeState
        //
        let ms_mass = Mass.ofMZ isolationWindowTargetMz fCharge

        //
        let ides = 
            possiblePeptideInfos
            |> List.fold (fun acc lookUpResult -> 
                                let sequence = lookUpResult.BioSequence
                                let seqL = sequence.Length
                                // 
                                let theoSpec = 
                                    predictOf BioFSharp.Formula.monoisoMass scanlimits sequence fCharge
                                // Calculates score for real sequence
                                let targetAndroScore = scoreTheoVsRecordedSpec scanlimits matchingTolPPM theoSpec binnedRecSpec
                                let targetAndroResult = createAndromedaScore spectrumID lookUpResult.ModSequenceID lookUpResult.PepSequenceID lookUpResult.GlobalMod true lookUpResult.StringSequence chargeState isolationWindowTargetMz ms_mass lookUpResult.Mass seqL targetAndroScore nan

                                // Calculates score for reversed decoy sequence
                                let decoySec = sequence |> List.rev
                                let theoSpecDecoy = predictOf BioFSharp.Formula.monoisoMass scanlimits decoySec fCharge
                                let decoyAndroScore =  scoreTheoVsRecordedSpec scanlimits matchingTolPPM theoSpecDecoy binnedRecSpec
                                let decoyAndroResult = createAndromedaScore spectrumID lookUpResult.ModSequenceID lookUpResult.PepSequenceID lookUpResult.GlobalMod false lookUpResult.StringSequence chargeState isolationWindowTargetMz ms_mass lookUpResult.Mass seqL decoyAndroScore nan 
                                targetAndroResult :: decoyAndroResult :: acc                        
                         ) []
        calcDeltaAndorScore (ides  |> List.sortBy (fun sls -> - sls.AndroScore))  
