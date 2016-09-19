namespace BioFSharp.Mz

type PeakArray<[<EqualityConditionalOn; ComparisonConditionalOn >]'a when 'a :> IPeak> = array<'a>

module PeakArray =

    let map f pkarr : PeakArray<_> = 
        Array.map f pkarr

    let zipMzInt (mz:array<float>) (intensity:array<float>) : PeakArray<_> = 
        Array.map2 (fun m i -> Peak(m,i)) mz intensity

    let unzipMzInt (pkarr : PeakArray<_>) = 
        let n = pkarr.Length
        let mz     = Array.zeroCreate n
        let intens = Array.zeroCreate n
        for i=0 to n do
            mz.[i]     <- pkarr.[i]
            intens.[i] <- pkarr.[i]
        mz,intens
    
        
    /// Bins peaks to their next upper 1 Da bin
    let binToUpperIntergerMass (pkarr:PeakArray<_>) (minMassBoarder:int) (maxMassBoarder:int) = 
        let maxIndex = maxMassBoarder - minMassBoarder + 1
        let array = Array.zeroCreate (maxIndex)
        pkarr 
        |> Array.iter (fun p ->
            let index = int(ceil p.Mz) - minMassBoarder
            if index < maxIndex && index > -1 then
                array.[index] <- max array.[index] p.Intensity)
        array

    /// Bins peaks to their nearest 1 Da bin
    let peaksToNearestUnitDaltonBin (pkarr:PeakArray<_>) (minMassBoarder:int) (maxMassBoarder:int) = 
        let maxIndex = maxMassBoarder - minMassBoarder + 1        
        let array = Array.zeroCreate (maxIndex)
        pkarr 
        |> Array.iter (fun p ->  
            let index = int(round p.Mz) - minMassBoarder
            if index < maxIndex && index > -1 then
                array.[index] <- max array.[index] p.Intensity)
        array
         