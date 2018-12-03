namespace BioFSharp

///Contains functionality for working with and creating isotopes and list of biologically relevant isotopes
module Isotopes = 
    
    ///Representation of Isotopes
    type Isotope = { AtomicSymbol  : string;
                     AtomicNumberZ : int;
                     MassNumber    : int;
                     Mass          : float;
                     NatAbundance  : float;
                     RelAtomicMass : float;
                    }

    ///Creates an isotope out of the given information about the isotope
    let create atomicSymbol atomicNumberZ massNumber mass natAbundance relAtomicMass =
        { AtomicSymbol = atomicSymbol; AtomicNumberZ = atomicNumberZ; MassNumber =massNumber ; Mass = mass; NatAbundance = natAbundance; RelAtomicMass = relAtomicMass; }

    ///Contains the stable isotopes of biologically relevant chemical elements.
    ///
    ///Note that the name convention of isotopes e.g. 15N cannot be satisfied here, 
    ///as name bindings starting with numbers are not allowed. 
    ///
    ///Therefore, numbers and element symbols are switched, e.g. 15N becoming N15
    module Table =
        //according to http://www.ciaaw.org/
        //===================== Hydrogen =========================
        ///Hydrogen whose nucleus consist of 1 proton
        let H1   = create "H" 1 1 1.00782503207 0.999885 1.007947
        ///Hydrogen whose nucleus consist of 1 proton and 1 neutron
        let H2   = create "D" 1 2 2.0141017778  0.000115 1.007947
        ///Hydrogen whose nucleus consist of 1 proton and 2 neutrons
        let H3   = create "T" 1 3 3.0160492777  nan      1.007947

        //===================== Carbon ===========================
        ///Carbon whose nucleus consist of 6 protons and 6 neutrons
        let C12  = create "C" 6 12 12.    0.98938 12.01078
        ///Carbon whose nucleus consist of 6 protons and 7 neutrons
        let C13  = create "C" 6 13 13.0033548378 0.01078 12.01078 
        ///Carbon whose nucleus consist of 6 protons and 8 neutrons
        let C14  = create "C" 6 14 14.0032419894 nan     12.01078

        //===================== Nitrogen =========================
        ///Nitrogen whose nucleus consist of 7 protons and 7 neutrons
        let N14  = create "N" 7 14 14.0030740048 0.99636 14.0067
        ///Nitrogen whose nucleus consist of 7 protons and 8 neutrons
        let N15  = create "N" 7 15 15.0001088982 0.00364 14.0067

        //===================== Oxygen ===========================
        ///Oxygen whose nucleus consist of 8 protons and 8 neutrons
        let O16  = create "O" 8 16 15.994914619 0.99757  15.9994
        ///Oxygen whose nucleus consist of 8 protons and 9 neutrons
        let O17  = create "O" 8 17 16.99913170  0.00038  15.9994
        ///Oxygen whose nucleus consist of 8 protons and 10 neutrons
        let O18  = create "O" 8 18 17.9991611   0.00205  15.9994

        //===================== Sodium ===========================
        ///Sodium whose nucleus consist of 11 protons and 12 neutrons
        let Na23 = create "Na" 11 23 22.98976928 1. 22.98976928

        //===================== Magnesium ========================
        ///Magnesium whose nucleus consist of 12 proton and 12 neutrons
        let Mg24 = create "Mg" 12 24 23.98504170 0.7888  24.304
        ///Magnesium whose nucleus consist of 12 proton and 13 neutrons
        let Mg25 = create "Mg" 12 25 24.9858370  0.09988 24.304
        ///Magnesium whose nucleus consist of 12 proton and 14 neutrons
        let Mg26 = create "Mg" 12 26 25.9825930  0.1096  24.304

        //===================== Phopsphorus ======================
        ///Phopsphorus whose nucleus consist of 15 proton and 16 neutrons
        let P31  = create "P" 15 31 30.97376163 1.       30.973762

        //===================== Sulfur ===========================
        ///Sulfur whose nucleus consist of 16 protons and 16 neutrons
        let S32  = create "S" 16 32 31.972071   0.9499   32.066
        ///Sulfur whose nucleus consist of 16 protons and 17 neutrons
        let S33  = create "S" 16 33 32.97145876 0.0075   32.066
        ///Sulfur whose nucleus consist of 16 protons and 18 neutrons
        let S34  = create "S" 16 34 33.96786690 0.0425   32.066
        ///Sulfur whose nucleus consist of 16 protons and 20 neutrons
        let S36  = create "S" 16 36 35.96708076 0.0001   32.066

        // Chlorine
        //let Cl35 = create "Cl" 17 35 34.9688527 0.755 35.457
        //let Cl37 = create "Cl" 17 37 36.9659026 0.245 35.457

        //===================== Potassium ========================
        ///Potassium whose nucleus consist of 19 protons and 0 neutrons
        let K39 = create "K" 19 39 38.96370649 0.932581 39.0983
        ///Potassium whose nucleus consist of 19 protons and 21 neutrons
        let K40 = create "K" 19 40 39.9639982  0.000117 39.0983
        ///Potassium whose nucleus consist of 19 protons and 22 neutrons
        let K41 = create "K" 19 41 40.96182526 0.067302 39.0983

        //===================== Calcium ==========================
        ///Calcium whose nucleus consist of 20 protons and 20 neutrons
        let Ca40 = create "Ca" 20 40 39.9625909 0.96941 40.078
        ///Calcium whose nucleus consist of 20 protons and 22 neutrons
        let Ca42 = create "Ca" 20 42 41.958618 0.00647 40.078
        ///Calcium whose nucleus consist of 20 protons and 23 neutrons
        let Ca43 = create "Ca" 20 43 42.958766 0.00135 40.078
        ///Calcium whose nucleus consist of 20 protons and 24 neutrons
        let Ca44 = create "Ca" 20 44 43.955482 0.02086 40.078
        ///Calcium whose nucleus consist of 20 protons and 26 neutrons
        let Ca46 = create "Ca" 20 46 45.95369 0.00004 40.078
        ///Calcium whose nucleus consist of 20 protons and 28 neutrons
        let Ca48 = create "Ca" 20 48 47.9525228 0.00187 40.078

        //===================== Iron =============================
        ///Iron whose nucleus consist of 26 protons and 28 neutrons
        let Fe54 = create "Fe" 26 54 53.939609 0.05845 55.845
        ///Iron whose nucleus consist of 26 protons and 30 neutrons
        let Fe56 = create "Fe" 26 56 55.934936 0.91754 55.845
        ///Iron whose nucleus consist of 26 protons and 31 neutrons
        let Fe57 = create "Fe" 26 57 56.935393 0.02119 55.845
        ///Iron whose nucleus consist of 26 protons and 32 neutrons
        let Fe58 = create "Fe" 26 58 57.933274 0.00282 55.845

        //===================== Copper ===========================
        ///Copper whose nucleus consist of 29 protons and 34 neutrons
        let Cu63 = create "Cu" 29 63 62.929598 0.6915 63.546
        ///Copper whose nucleus consist of 29 protons and 36 neutrons
        let Cu65 = create "Cu" 29 65 64.927790 0.3085 63.546

        //===================== Zinc =============================
        ///Zinc whose nucleus consist of 30 protons and 34 neutrons
        let Zn64 = create "Zn" 30 64 63.929142 0.4917 65.38
        ///Zinc whose nucleus consist of 30 protons and 36 neutrons
        let Zn66 = create "Zn" 30 66 65.926034 0.2773 65.38
        ///Zinc whose nucleus consist of 30 protons and 37 neutrons
        let Zn67 = create "Zn" 30 67 66.927128 0.0404 65.38
        ///Zinc whose nucleus consist of 30 protons and 38 neutrons
        let Zn68 = create "Zn" 30 68 67.924845 0.1845 65.38
        ///Zinc whose nucleus consist of 30 protons and 40 neutrons
        let Zn70 = create "Zn" 30 70 69.92532  0.0061 65.38

        //===================== Selenium =========================
        ///Selenium whose nucleus consist of 34 protons and 40 neutrons
        let Se74 = create "Se" 34 74 73.9224764 0.0089 78.96
        ///Selenium whose nucleus consist of 34 protons and 42 neutrons
        let Se76 = create "Se" 34 76 75.9192136 0.0937 78.96
        ///Selenium whose nucleus consist of 34 protons and 43 neutrons
        let Se77 = create "Se" 34 77 76.9199140 0.0763 78.96
        ///Selenium whose nucleus consist of 34 protons and 44 neutrons
        let Se78 = create "Se" 34 78 77.9173091 0.2377 78.96
        ///Selenium whose nucleus consist of 34 protons and 46 neutrons
        let Se80 = create "Se" 34 80 79.9165213 0.4961 78.96
        ///Selenium whose nucleus consist of 34 protons and 48 neutrons
        let Se82 = create "Se" 34 82 81.9166994 0.0873 78.96






