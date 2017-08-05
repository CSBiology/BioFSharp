namespace BioFSharp

module Isotopes = 
    
    type Isotope = { AtomicSymbol  : string;
                     AtomicNumberZ : int;
                     MassNumber    : int;
                     Mass          : float;
                     NatAbundance  : float;
                     RelAtomicMass : float;
                    }

    let create atomicSymbol atomicNumberZ massNumber mass natAbundance relAtomicMass =
        { AtomicSymbol = atomicSymbol; AtomicNumberZ = atomicNumberZ; MassNumber =massNumber ; Mass = mass; NatAbundance = natAbundance; RelAtomicMass = relAtomicMass; }

    
    module Table =
        
        // Hydrogen
        let H1   = create "H" 1 1 1.00782503207 0.999885 1.007947
        let H2   = create "D" 1 2 2.0141017778  0.000115 1.007947
        let H3   = create "T" 1 3 3.0160492777  nan      1.007947
        // Phosphorus
        let P31  = create "P" 15 31 30.97376163 1.       30.973762
        // Carbon
        let C12  = create "C" 6 12 12.0000000    0.98938 12.01078
        let C13  = create "C" 6 13 13.0033548378 0.01078 12.01078 
        let C14  = create "C" 6 14 14.0032419894 nan     12.01078
        // Nitrogen
        let N14  = create "N" 7 14 14.0030740048 0.99636 14.0067
        let N15  = create "N" 7 15 15.0001088982 0.00364 14.0067
        // Oxygen
        let O16  = create "O" 8 16 15.994914619 0.99757  15.9994
        let O17  = create "O" 8 17 16.99913170  0.00038  15.9994
        let O18  = create "O" 8 18 17.9991611   0.00205  15.9994
        // Sulfur
        let S32  = create "S" 16 32 31.972071   0.9499   32.066
        let S33  = create "S" 16 33 32.97145876 0.0075   32.066
        let S34  = create "S" 16 34 33.96786690 0.0425   32.066
        let S36  = create "S" 16 36 35.96708076 0.0001   32.066
        // Selenium
        let Se74 = create "Se" 34 74 73.9224764 0.0089 78.96
        let Se76 = create "Se" 34 76 75.9192136 0.0937 78.96
        let Se77 = create "Se" 34 77 76.9199140 0.0763 78.96
        let Se78 = create "Se" 34 78 77.9173091 0.2377 78.96
        let Se80 = create "Se" 34 80 79.9165213 0.4961 78.96
        let Se82 = create "Se" 34 82 81.9166994 0.0873 78.96


