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
    
        

         