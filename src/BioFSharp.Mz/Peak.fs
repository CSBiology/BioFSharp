namespace BioFSharp.Mz


type IPeak =    
    abstract member Mz        : float
    abstract member Intensity : float   


[<Struct>]
type Peak(mz:float,intensity:float) = 
        member this.Mz = mz
        member this.Intensity = intensity
        
        interface IPeak with
            member this.Mz = mz
            member this.Intensity = intensity

module Peaks =
 
    let createPeak mzData intensityData = 
        let pk = new Peak(mzData, intensityData)
        pk