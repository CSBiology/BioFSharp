namespace BioFSharp

module Mass =
    
    open BioFSharp
   

    module Table = 
        
        /// <summary>
        /// Proton Mass. Units u 
        /// </summary>
        [<Literal>]
        let PMassInU =  1.00727646677 // Wiki: 1,007 276 466 812(90) u
        
    /// <summary>
    ///   Converts mass to m/z
    /// </summary>
    ///
    /// <param name="mass">Mass m</param>    
    /// <param name="z">Charge z</param>
    /// <returns>Returns m/z</returns> 
    let toMZ (mass:float) (z:float) =        
        (mass + (Table.PMassInU * z))  / z
    

    /// <summary>
    ///   Converts m/z to neutral mass
    /// </summary>
    ///
    /// <param name="mass">Mass-charge ratio m/z</param>    
    /// <param name="z">Charge z</param>
    /// <returns>Returns neutral mass</returns> 
    let ofMZ (mz:float) (z:float) = 
        ((mz * z) - (Table.PMassInU * z))

    /// Calculates accuracy of mass versus (theoretical) reference mass 
    let accuracy mass referenceMass =
        ((mass - referenceMass) / referenceMass) * 1000000.


    /// Returns delta mass by ppm
    let deltaMassByPpm ppm mass =
        mass * 0.000001 * ppm

    /// Returns -/+ mass range 
    let rangePpm ppm mass =
        let deltaM = deltaMassByPpm ppm mass
        (mass - deltaM,mass + deltaM)


