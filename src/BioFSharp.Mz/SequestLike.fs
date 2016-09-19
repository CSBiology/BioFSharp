namespace BioFSharp.Mz

open BioFSharp
open Fragmentation

open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra.Double

module SequestLike =

    type SequestLikeScore =
        { SpectrumID      : string;
          PrecursorCharge : int;
          IsTarget        : bool;
          Peptide         : seq<AminoAcids.AminoAcid>;
          TheoMass        : float;      
          MeasuredMass    : float;      
          PeptideLength   : int;
          XCORR           : float;
          DeltaCN         : float; }

    let createSequestLikeScore spectrumID precursorCharge isTarget peptide theoMass measuredMass peptideLength xcorr deltaCN = 
        { SpectrumID = spectrumID; PrecursorCharge = precursorCharge; IsTarget = isTarget; Peptide = peptide; 
          TheoMass = theoMass; MeasuredMass = measuredMass; PeptideLength = peptideLength; XCORR = xcorr; DeltaCN = deltaCN; }



    /// normalize the intensities within a window to maximum of the window
    /// Attention shortens the array  (cuts)
    let windowNormalizeIntensities (intensities:float[]) (numberOfWindows:int) =
        // finds max within range
        let rec findMax (array:float[]) (cMax:float) (lowerLimit:int) (counter:int)  =
            if counter < lowerLimit then
                cMax
            else
                let nMax = max array.[counter] cMax
                findMax array nMax lowerLimit (counter - 1)

        // sqrt intensities and normalize to maximum within lower-upper limit
        let normToMaxSqrtInPlace (array:float[]) (lowerLimit:int) (upperLimit:int)  =
            let nMax = sqrt (findMax array 0.0 lowerLimit upperLimit)
            for i = lowerLimit to upperLimit do
                if nMax > 0. then                
                    array.[i] <- (sqrt array.[i]) / nMax 
                else
                    array.[i] <- 0.


        let windowSize =  (intensities.Length / numberOfWindows)    
        let tmpIntensities = 
            Array.init (windowSize*numberOfWindows) 
                (fun i -> intensities.[i])
    
        for i = 1 to numberOfWindows do
            //printfn "window: %i lower: %i counter: %i " i (windowSize * (i - 1)) (windowSize * i - 1)
            normToMaxSqrtInPlace tmpIntensities (windowSize * (i - 1)) (windowSize * i - 1) 
        tmpIntensities


    let predictIntensitySimpleModel (iontype:IonTypes) (charge:float) =
        match iontype with
        | Unknown   -> 1.  / charge
        | Precursor -> 1.  / charge
        | A         -> 0.2 / charge
        | B         -> 1.  / charge
        | C         -> 0.2 / charge 
        | X         -> 0.2 / charge 
        | Y         -> 1.  / charge 
        | Z         -> 0.2 / charge 
        | AlossH2O  -> 0.2 / charge  
        | AlossNH3  -> 0.2 / charge 
        | BlossH2O  -> 0.2 / charge 
        | BlossNH3  -> 0.2 / charge
        | ClossH2O  -> 0.2 / charge
        | ClossNH3  -> 0.2 / charge 
        | XlossH2O  -> 0.2 / charge
        | XlossNH3  -> 0.2 / charge 
        | YlossH2O  -> 0.2 / charge 
        | YlossNH3  -> 0.2 / charge  
        | ZlossH2O  -> 0.2 / charge 
        | ZlossNH3  -> 0.2 / charge 
        | Immonium  -> 0.6 / charge 
        | _         -> 1.  / charge
    
    
    let private bs (massfunction:Formula.Formula -> float) (aal:AminoAcids.AminoAcid list) = 
        let rec series aminoList fragMasses acc =
            match aminoList with
            | f::rest    -> let currentMass = massfunction (AminoAcids.formula f)                            
                            //let a' = acc + currentMass - (massDiffAX_CO (AminoAcids.getLabel f).Label )
                            let b' = acc + currentMass
                            let bN = acc + currentMass - (massfunction (AminoAcids.isotopicLabelFunc f Formula.Table.NH3))
                            let bH = if isWaterLoss f then (acc + currentMass - (massfunction (AminoAcids.isotopicLabelFunc f Formula.Table.H2O))) else nan
                            series rest ((b',bN,bH)::fragMasses) b'
            | _          ->  fragMasses
        
        (series aal [] 0.0) |> List.rev
        

    let private ys (massfunction:Formula.Formula -> float) (aal:AminoAcids.AminoAcid list) = 
        let rec series aminoList fragMasses acc =
            match aminoList with
            | f::rest -> let currentMass = massfunction (AminoAcids.formula f)
                         let y' = acc + currentMass 
                         let yN = if isAminoLoss f then (acc + currentMass - ((massfunction (AminoAcids.isotopicLabelFunc f Formula.Table.NH3)))) else nan
                         let yH = acc + currentMass - (massfunction (AminoAcids.isotopicLabelFunc f Formula.Table.H2O))
                         series (rest) ((y',yN, yH)::fragMasses) y'
            | _          -> fragMasses
        (series (aal|> List.rev) [] (massfunction Formula.Table.H2O))       



    let predictOf (massfunction:Formula.Formula -> float) (aa:seq<AminoAcids.AminoAcid>) (maxcharge:float) =
        let aal      = aa |> Seq.toList
        //a series missing
        let b'bNbH_list = bs massfunction aal
        let y'yNyH_list = ys massfunction aal        

        let rec recloop (ions) (charge) (b'bNbH_list:List<float*float*float>) (y'yNyH_list:List<float*float*float>) =
            match b'bNbH_list,y'yNyH_list with
            | (b,bN,bH)::restb,(y,yN,yH)::resty ->  let b'  = Peak( (Mass.toMZ b charge) , (predictIntensitySimpleModel IonTypes.B charge)        )
                                                    let bN' = Peak( (Mass.toMZ bN charge), (predictIntensitySimpleModel IonTypes.BlossNH3 charge) )
                                                    let bH' = Peak( (Mass.toMZ bH charge), (predictIntensitySimpleModel IonTypes.BlossH2O charge) )
                                                                                                                                                 
                                                    let y'  = Peak( (Mass.toMZ y charge) , (predictIntensitySimpleModel IonTypes.Y charge)        )
                                                    let yN' = Peak( (Mass.toMZ yN charge), (predictIntensitySimpleModel IonTypes.YlossNH3 charge) )
                                                    let yH' = Peak( (Mass.toMZ yH charge), (predictIntensitySimpleModel IonTypes.YlossH2O charge) )

                                                    recloop (b'::bN'::bH'::y'::yN'::yH'::ions) charge restb resty
            | [],_ -> ions
            | _,[] -> ions
            
            
        seq { for z = 1. to maxcharge do
                yield! recloop [] z b'bNbH_list y'yNyH_list }      
    
    
    
    let shiftedVectorSum (plusMinusMaxDelay:int) (array:LinearAlgebra.Vector<float>) =
        let shifted (array:LinearAlgebra.Vector<float>) (tau:int) =
            array
            |> LinearAlgebra.Vector.mapi
                (fun i x ->
                    let index = i - tau
                    if (index < 0) || (index > array.Count - 1) then 
                        0.
                    else
                        array.[index] )
        let rec accumVector (accum) (state:int) (max:int) =
            if state = max then
                accum
            else
                accumVector (accum + (shifted array state)) (state - 1) (max)
        let emtyVector = DenseVector (Array.zeroCreate array.Count)
        let plus  = accumVector emtyVector (plusMinusMaxDelay) (1)
        let minus = accumVector emtyVector (-1) (-plusMinusMaxDelay)
        (plus + minus)
        |> LinearAlgebra.Vector.map (fun x ->  x / (float plusMinusMaxDelay * 2.))
    
    
    
    let createShiftedMatrix (plusMinusMaxDelay:int) (array:float[]) =
        let colNumber = array.Length
        Array2D.init (plusMinusMaxDelay * 2 + 1) colNumber
            (fun i ii ->
                let ni = (i - plusMinusMaxDelay)
                let index = ii - ni
                if (index < 0) || (index > colNumber - 1) then 
                    0.
                else
                    array.[index] )



    /// Amino acid sequence (peptide) to sequest-like predicted intensity array
    let peptideToNormalizedIntensityArray (massfunction:Formula.Formula -> float) (scanlimits:int*int) charge peptide =
        let lowerScanLimit,upperScanLimit = scanlimits           
        let psi  = predictOf massfunction peptide charge |> Seq.toArray
        let npsi = PeakArray.peaksToNearestUnitDaltonBin psi lowerScanLimit upperScanLimit        
        npsi


    /// Measured spectrum to sequest-like normalized intensity array
    /// ! Uses 10 as number of windows for window normalization (like in original sequest algorithm)    
    let spectrumToNormalizedIntensityArray (scanlimits:int*int) (spectrum:PeakArray<_>) =
        let lowerScanLimit,upperScanLimit = scanlimits    
        let si  = PeakArray.peaksToNearestUnitDaltonBin spectrum lowerScanLimit upperScanLimit
        let nsi = windowNormalizeIntensities si 10 // |> Seq.toArray
        nsi


    /// Measured spectrum to sequest-like normalized intensity array
    /// minus auto-correlation (delay 75 -> like in original sequest algorithm)
    /// ! Uses 10 as number of windows for window normalization (like in original sequest algorithm)    
    let spectrumToIntensityArrayMinusAutoCorrelation (scanlimits:int*int) (spectrum:PeakArray<_>) =
        let lowerScanLimit,upperScanLimit = scanlimits    
        let si  = PeakArray.peaksToNearestUnitDaltonBin spectrum lowerScanLimit upperScanLimit
        let nsi = windowNormalizeIntensities si 10 |> DenseVector.OfEnumerable                
        let nsi' = shiftedVectorSum 75 nsi
        (nsi - nsi')  


    /// Calculates sequest-like deltaCN score
    ///  (Xcorr(top hit) - Xcorr(n)) ÷ Xcorr(top hit). Thus, the deltaCn for the top hit is
    ///  (Xcorr(top hit) - Xcorr(top hit)) ÷ Xcorr(top hit) = 0.
    let calcDeltaCN (sourceList:SequestLikeScore list) =
        match sourceList with
        | h1::rest -> 
            sourceList
            |> List.map
                (fun sls ->
                    let deltaCN = (h1.XCORR - sls.XCORR) / h1.XCORR
                    { sls with DeltaCN = deltaCN } )      
        | []       -> []






    //scanNumber precursorCharge isTarget peptide theoMass measuredMass peptideLength xcorr deltaCN = 
    let calcSequestLikeScoresRevDecoy (massfunction:Formula.Formula -> float) (scanlimits) (spectrum:PeakArray<_>) chargeState isolationWindowTargetMz (possiblePeptideInfos:seq<float*BioArray.BioArray<_>>) spectrumID = // (scan:Spectra.Scan) (possiblePeptidesInfoGroups:list<Mz.PeptideLookUp.PeptideInfoGroup>) =
        // measured normailzed intensity array (spectrum) minus auto-correlation
        let ms_nis =  spectrumToIntensityArrayMinusAutoCorrelation scanlimits spectrum
        // float charge
        let fCharge = float chargeState
        // measured mass
        let ms_mass = Mass.ofMZ isolationWindowTargetMz fCharge


        let ides = [ for mass,sequence in possiblePeptideInfos do         
                        //predicted  normailzed intensity array (spectrum) 
                        let p_nis = peptideToNormalizedIntensityArray massfunction scanlimits fCharge sequence |> DenseVector.OfArray
                        let xcorr = p_nis * ms_nis
                        yield createSequestLikeScore spectrumID chargeState true sequence mass ms_mass (Array.length sequence) xcorr nan
                        
                        let revPeptide_decoy = sequence |> Array.rev
                        let p_nis_decoy      = peptideToNormalizedIntensityArray massfunction scanlimits fCharge revPeptide_decoy |> DenseVector.OfArray
                        let xcorr_decoy      = p_nis_decoy * ms_nis
                        yield createSequestLikeScore spectrumID chargeState false revPeptide_decoy mass ms_mass revPeptide_decoy.Length xcorr_decoy nan ]
                    
        calcDeltaCN (ides  |> List.sortBy (fun sls -> - sls.XCORR))
        




