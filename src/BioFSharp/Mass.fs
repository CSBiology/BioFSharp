namespace BioFSharp

///Molecular mass related functions
module Mass =
    
    open BioFSharp
   
    ///Nucleon masses
    module Table = 
        
        /// <summary>
        /// Proton Mass. Units u 
        /// </summary>
        [<Literal>]
        let PMassInU =  1.00727646681 // Wiki: 1.007 276 466 812(90) u
                        
        /// <summary>
        /// Neutron Mass. Units u 
        /// </summary>
        [<Literal>]
        let NMassInU =  1.00866491588 // Wiki: 1.008 664 915 88(49) u
        
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

    /// Calculates accuracy in ppm of mass versus (theoretical) reference mass 
    let accuracy mass referenceMass =
        ((mass - referenceMass) / referenceMass) * 1000000.

    /// Returns delta mass by ppm
    let deltaMassByPpm ppm mass =
        mass * 0.000001 * ppm

    /// Returns -/+ mass range 
    let rangePpm ppm mass =
        let deltaM = deltaMassByPpm ppm mass
        (mass - deltaM,mass + deltaM)


