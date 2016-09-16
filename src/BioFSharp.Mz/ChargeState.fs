namespace BioFSharp.Mz

module pimp =
    let x = 2

    open BioFSharp
    open System
    open FSharp.Care
    
    open MathNet.Numerics

module ChargeState =
    
    open SignalDetection

    type PeakWindowParams = {
        Width: float
        /// RelativeToStartPeak
        MinIntensity: float
        /// RelativeToPriorPeak
        DeltaMinIntensity: float
        }
    
    let createPeakWindowParams width minIntensity deltaMinIntensity = {
        Width = width; MinIntensity = minIntensity; DeltaMinIntensity = deltaMinIntensity}

    type ChargeDetermParams = {
        ExpectedMinimalCharge : int ///TODO: learn from Data
        ExpectedMaximumCharge : int ///TODO: learn from Data
        PeakWindowThresholds  : PeakWindowParams
        NrOfRndSpectra        : int
        }

    let createChargeDetermParams expectedMinimalCharge expectedMaximumCharge peakWindowThresholds nrOfRndSpectra = {
        ExpectedMinimalCharge=expectedMinimalCharge; ExpectedMaximumCharge=expectedMaximumCharge; PeakWindowThresholds=peakWindowThresholds; NrOfRndSpectra=nrOfRndSpectra }

    type AssignedCharge = { 
        Charge                      : int
        MZChargeDev                 : float
        Score                       : float
        DistanceRealTheoPeakSpacing : float list
        SubSetLength           : int 
        StartPeakIntensity          : float
        PeakSourcePositions         : Set<int>
        } 

    let createAssignedCharge charge mZChargeDev score distanceRealTheoPeakSpacing subSetLength startPeakIntensity peakSourcePositions= {
        Charge=charge; MZChargeDev=mZChargeDev; Score=score;DistanceRealTheoPeakSpacing=distanceRealTheoPeakSpacing; 
            SubSetLength=subSetLength; StartPeakIntensity=startPeakIntensity; PeakSourcePositions=peakSourcePositions }
   
    type TestedItem<'a> = {
        TestedObject: 'a
        PValue: float
        }     
    
    let createTestedItem testedObject pValue = {
        TestedObject=testedObject; PValue=pValue }
 
    /// Returns Index of the highestPeak flanking a given mzValue
    let idxOfHighestPeakBy (sortedMZIntensityArray: MzIntensityArray) mzValue = 
        let idxHigh = 
            sortedMZIntensityArray.MzData 
            |> Array.tryFindIndex (fun x -> x > mzValue) // faster as binary search
        let idxLow = 
            match idxHigh with 
            | None   -> Some (sortedMZIntensityArray.MzData.Length-1) 
            | Some value -> match value with 
                            | 0 -> None
                            | _ -> Some (value-1)  
        if idxLow = None then 
             idxHigh.Value
        elif idxHigh = None then 
             idxLow.Value
        else
            if sortedMZIntensityArray.IntensityData.[idxLow.Value] > sortedMZIntensityArray.IntensityData.[idxHigh.Value] then 
                 idxLow.Value
            else idxHigh.Value    

    /// Returns a Collection of MZIntensityPeaks, The Collection starts with the first Element on the right side of the startIdx. 
    /// and ends either with the last element of the mzIntensityArray or when the MzDistance to the highest Peak exceeds 
    /// the given windowwidth.   
    let getRelPeakPosInWindowBy (mzIntensityArray:MzIntensityArray) (thresholds: PeakWindowParams) mzValue startIdx =
        let mzDataLength = mzIntensityArray.MzData.Length
        if mzDataLength > 0 then
            let startPkMZ = mzIntensityArray.MzData.[startIdx]
            let startPkInt = mzIntensityArray.IntensityData.[startIdx]
            let hasValidIntensity count =
                mzIntensityArray.IntensityData.[count] > thresholds.MinIntensity * startPkInt &&
                mzIntensityArray.IntensityData.[count] > thresholds.DeltaMinIntensity *  mzIntensityArray.IntensityData.[count-1]   
            let rec loop count accmz (mzIntensityArray:MzIntensityArray) =            
                if count = mzDataLength then 
                     accmz
                elif mzIntensityArray.MzData.[count] - startPkMZ > thresholds.Width then 
                     accmz
                elif count = mzDataLength-1 && hasValidIntensity count then 
                     loop (count+1) (createMzIntensityPeak (mzIntensityArray.MzData.[count]-startPkMZ) (mzIntensityArray.IntensityData.[count] / startPkInt) (Some (count-startIdx))::accmz) mzIntensityArray
                elif hasValidIntensity count then 
                     loop (count+1) (createMzIntensityPeak (mzIntensityArray.MzData.[count]-startPkMZ) (mzIntensityArray.IntensityData.[count] / startPkInt) (Some (count-startIdx))::accmz) mzIntensityArray
                else loop (count+1) accmz mzIntensityArray
            loop (startIdx+1) [] mzIntensityArray
            |> fun sourceSet -> startPkInt ,createMzIntensityPeakCollection sourceSet (sourceSet.Length+1) (sourceSet.Length+1) //the length must be raised by 1 because the first element (0.,1.) is left out from the collection 
        else 0., createMzIntensityPeakCollection [] 0 0
    
    /// Creates the PowerSet of a given Collection of MZIntensityPeaks. Adds a StartPeak with the relative Position 0 and the 
    /// relative Intensity 1 to each subSet. 
    let powerSetOf (mzIntensityPeaC: MzIntensityPeakCollection) = 
        let rec createPowerset (inputL: MzIntensityPeak list) = 
            match inputL with
            | [] -> [[createMzIntensityPeak 0. 1. (Some 0)]] 
            | x::xs -> List.collect (fun subSet ->
                                                    [subSet; x::subSet]
                                    ) (createPowerset xs)
        let mzIntensityPeaCPwrSet = createPowerset (mzIntensityPeaC.MzIntensityPeaks) 
        mzIntensityPeaCPwrSet
        |> List.map (fun subSetmzIntensityPeaC -> createMzIntensityPeakCollection subSetmzIntensityPeaC subSetmzIntensityPeaC.Length mzIntensityPeaC.PeaksInSourceSet)
   
    /// Calculates the mzDistances of a List of MzIntensityPeaks
    let mzDistancesOf  (mzIntensityPeaks: MzIntensityPeak list) = //TODO: calc Slope; times Slope changed: Timo fragen ob gewollt. 
        let rec innerF acc (mzIntensityPeaks: MzIntensityPeak list) =
            match mzIntensityPeaks with 
            | []      -> acc
            | h::[]   -> acc 
            | h::tail -> innerF (h.MzData - tail.Head.MzData::acc) tail
        innerF [] mzIntensityPeaks

    /// Returns a charge state based on a given meanOfInterPeakDistances. The list of possible charge states is defined by
    /// the user.  
    let getChargeBy (chargeDeterminationParams: ChargeDetermParams) (meanOfInterPeakDistances: float) = 
        let chargeL = [chargeDeterminationParams.ExpectedMinimalCharge.. chargeDeterminationParams.ExpectedMaximumCharge]
        let rec innerF (acc:int*float) (chargeL: int list) (meanOfPeakDistances:float) = 
                match chargeL with 
                | [] -> fst acc
                | h::tail -> let theoreticalInterIsotopeDistance = 1./ float h
                             let difMeanPeakDistanceToTheoDistance = meanOfPeakDistances-theoreticalInterIsotopeDistance |> abs
                             if theoreticalInterIsotopeDistance <= meanOfPeakDistances && difMeanPeakDistanceToTheoDistance < snd acc then 
                                  h  
                             elif difMeanPeakDistanceToTheoDistance > snd acc then 
                                  (fst acc)
                             else innerF (h, difMeanPeakDistanceToTheoDistance ) tail meanOfPeakDistances 
        innerF (0, 100.) chargeL meanOfInterPeakDistances 

    /// Returns the MZChargeDeviation based on the theoreticalInterIsotopeDistance as a Measure for central tendency
    /// at a given ChargeState. 
    let mzChargeDeviationBy (interPeakDistances:seq<float>) (theoreticalInterIsotopeDistance)  =
        use e = interPeakDistances.GetEnumerator()
        let rec loop n (acc:float) =
            match e.MoveNext() with
            | true  -> loop (n + 1) (acc + ((square (e.Current - theoreticalInterIsotopeDistance) )/theoreticalInterIsotopeDistance))
            | false -> if (n > 0) then sqrt (acc / (float n)) else nan            
        loop 0 0.0 

    /// Returns a empirically determined Score to achieve a Ranking of SubSets. The optimal weighting of the parameters was
    /// determined via Linear Discriminant Analysis.  
    let getScore (elementsInSubSet:int) (elementsInSourceSet:int) deviationPeakDistanceToTheoDistance = 
        let ldaVec  = [|1.0; -0.30|]
        ldaVec.[0] * deviationPeakDistanceToTheoDistance + ldaVec.[1] * (float elementsInSubSet/ float elementsInSourceSet)  

    /// Returns a random integer between a lower and a upper Value
    let rndIntBetween (rnd: Random.MersenneTwister) lowerBorder upperBorder  = rnd.Next(lowerBorder, upperBorder)

    /// Returns a possible InterPeakDistance by a given charge state. The retrieved distance follows a normaldistribution
    /// centered around the theoretical interPeakDistance. The standardDeviation is dependent on the used mass spectrometer
    let interPeakDistanceBy (stdDev: float)  (assignedCharge: float) = 
        let calcRndPosition assignedCharge stdDev =
            1./assignedCharge + MathNet.Numerics.Distributions.Normal(0.,stdDev).Sample()  
        calcRndPosition assignedCharge stdDev

    /// Creates a random MzIntensityEntityCollection  
    let rndMzIntensityEntityCollectionBy (rnd: Random.MersenneTwister) (stdDev: float) (maxCharge: int) maxDistance minChainLength =
        let rec innerF count (accFloat:float) (accList:MzIntensityPeak list) minChainLength  =
            let rndCharge = 
                rndIntBetween rnd 1 maxCharge
                |> float 
            let nextRnd = (interPeakDistanceBy stdDev rndCharge)
            if (accFloat+nextRnd) > maxDistance then 
                 innerF 2 0. [createMzIntensityPeak 0. 1. None] (minChainLength)  
            elif count = minChainLength then 
                 createMzIntensityPeakCollection ((createMzIntensityPeak (accFloat+nextRnd) 0. None)::accList) minChainLength minChainLength    
            else innerF (count+1) (accFloat+nextRnd) ((createMzIntensityPeak (accFloat+nextRnd) 0. None)::accList) minChainLength     
        innerF 2 0. [createMzIntensityPeak 0. 1. None] (minChainLength)

        
    /// Creates a user defined amount of random spectra of defined length. Returns the mzChargeDeviation of each simulated Spectrum
    let generateMzSpecDev (rnd: Random.MersenneTwister) chargeStateDetermParams peakPosStdDev (chainLength,charge)  = 
        [1.. chargeStateDetermParams.NrOfRndSpectra] 
        |> List.map (fun i -> rndMzIntensityEntityCollectionBy (rnd: Random.MersenneTwister) peakPosStdDev chargeStateDetermParams.ExpectedMaximumCharge chargeStateDetermParams.PeakWindowThresholds.Width chainLength)           
        |> List.map (fun subSet -> 
                        let interPeakDistances = mzDistancesOf subSet.MzIntensityPeaks                    
                        let mzChargeDeviation = mzChargeDeviationBy interPeakDistances (1./charge)  
                        mzChargeDeviation
                        )
        |> List.sort
        |> Array.ofList
    
    /// Returns Function to generate random spectra and to calculate their mzChargeDeviations.   
    let initGenerateMzSpecDevWithMem (rnd:Random.MersenneTwister) (chargeStateDetermParams: ChargeDetermParams) peakPosStdDev =
        Memoization.memoize (generateMzSpecDev (rnd:Random.MersenneTwister) (chargeStateDetermParams: ChargeDetermParams) peakPosStdDev) ///stdv 0.01516580549
        
    /// Returns the empirically determined PValue. The PValue is the quotient of simulated mzChargeDeviations lower than the mzChargeDeviation
    /// observed divided by their total number
    let empiricalPValueOf initGenerateMzSpecDevWithMemF (nrOfPeaksInSubSet,charge) score  = //TODO nrOfPeaks,charge score in parameter
        let generateMzSpecDev = initGenerateMzSpecDevWithMemF (nrOfPeaksInSubSet,charge)
        let numerator =  (generateMzSpecDev |> Array.tryFindIndex (fun x -> x > score)) 
        match numerator with
        | Some x -> (float x) / float generateMzSpecDev.Length
        | None -> 1.

    /// Returns list of putative precursorChargeStates along with Properties used for evaluation.
    let putativePrecursorChargeStatesBy (chargeDeterminationParams: ChargeDetermParams) (centroidedSpectrum: MzIntensityArray) (precursorMZ:float) =
        let (startPeakIntensity,originSet) = getRelPeakPosInWindowBy centroidedSpectrum chargeDeterminationParams.PeakWindowThresholds precursorMZ (idxOfHighestPeakBy centroidedSpectrum precursorMZ)
        originSet
        |> powerSetOf 
        |> List.filter (fun subSet -> subSet.PeaksInSubSet > 1)
        |> List.map (fun subSet -> 
                        let peakPos = subSet.MzIntensityPeaks 
                                      |> List.map (fun x -> x.Position.Value) 
                                      |> Set.ofList
                        let interPeakDistances = mzDistancesOf subSet.MzIntensityPeaks 
                        let meanOfInterPeakDistances = Statistics.Statistics.Mean interPeakDistances
                        let assignedCharge = getChargeBy chargeDeterminationParams meanOfInterPeakDistances
                        let theoInterPeakDistances = 1. / float assignedCharge
                        let distanceRealTheoPeakSpacing = 
                            interPeakDistances
                            |> List.map (fun distance -> distance - theoInterPeakDistances ) 
                        let mzChargeDeviation = mzChargeDeviationBy interPeakDistances theoInterPeakDistances
                        let score = getScore subSet.PeaksInSubSet subSet.PeaksInSourceSet mzChargeDeviation
                        createAssignedCharge assignedCharge mzChargeDeviation score distanceRealTheoPeakSpacing subSet.PeaksInSubSet startPeakIntensity peakPos  
                     )
        |> List.sortBy (fun assignedCharge ->  assignedCharge.Score)
        |> List.distinctBy (fun assignedCharge ->  assignedCharge.Charge)
        
    /// Returns the StandardDeviation of the PeakDistances
    let peakPosStdDevBy (putativeChargeStates: AssignedCharge list) = 
        putativeChargeStates
        |> List.map (fun putativeChS -> putativeChS.DistanceRealTheoPeakSpacing )
        |> List.concat 
        |> Statistics.Statistics.StandardDeviation

    /// Returns a List of tested AssignedCharges. This Function eliminates all
    let removeSubSetsOfBestHit (assignedCharges: TestedItem<AssignedCharge> list) =
        let rec loop bestSet acc (assignedCharges: TestedItem<AssignedCharge> list) =
            match assignedCharges with 
            | [] -> (bestSet::acc) |> List.sortBy (fun x -> x.TestedObject.Score) 
            | h::tail -> match Set.isSuperset bestSet.TestedObject.PeakPositionInSourceSet h.TestedObject.PeakPositionInSourceSet with
                         | false  -> loop bestSet (h::acc) tail 
                         | true -> loop bestSet acc tail
        if assignedCharges = [] then 
            [] 
        else
            let bestSet = assignedCharges.Head
            loop bestSet [] assignedCharges

    /// Returns list of TestedItems of a Pvalue that meet the significance criteria