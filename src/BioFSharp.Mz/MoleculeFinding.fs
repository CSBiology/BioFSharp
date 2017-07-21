namespace BioFSharp.Mz

module MoleculeFinding =


    open BioFSharp
    open Isotopes.Table
    open AminoAcids

    type atom = {
        symbol : string
        mass : float;
        valence : int;
        }

    ///Hydrogen
    let H =  { symbol = "H" ; mass = H1.Mass  ; valence = 1 }
    ///Carbon
    let C =  { symbol = "C" ; mass = C12.Mass ; valence = 4 }
    ///Nitrogen
    let N =  { symbol = "N" ; mass = N14.Mass ; valence = 5 }
    ///Oxygen
    let O =  { symbol = "O" ; mass = O16.Mass ; valence = 6 }
    ///Phosphorus
    let P =  { symbol = "P" ; mass = 30.973762; valence = 5 }
    ///Sulfur
    let S =  { symbol = "S" ; mass = S32.Mass; valence = 6 }
    ///Selenium
    let Se = { symbol = "Se"; mass = Se74.Mass; valence = 6 }


    ///The pattern matching of this function checks if the float value "gap" is 0 +/- accuracy and if this is the case, the builded list of atoms (molecule) gets added to an accumulator list (acc). The list "molecule" is builded while the "gap" is bigger than the accuracy value and the number of valence electrons (counter) is between 1 and 9." This counter changes when choosing another atom from one of the predefined lists and gets substracted by one as often as the same atom is used. When "counter"=1 is reached another atom gets chosen from the predefined lists as long as the "gap" is not 0 +/- accuracy, because the "gap" gets substracted by the mass of each used atom. As result we get a list of potential molecule lists (acc).
    let find (allAtoms: atom list) (nodeAtoms: atom list) (gap: float) (accuracy: float) =
        let rec inner (allAtoms: atom list) (nodeAtoms: atom list) (gap: float) (accuracy: float) (molecule: atom list) (acc: atom list list) (counter: int) =
            allAtoms |> List.map (fun x ->
            
                match (gap,counter) with
                |(g,_) when g = 0. 
                
                    -> molecule::acc
            
                |(g,_) when g > 0. && g < accuracy 
            
                    -> molecule::acc
            
                |(g,_) when g < 0. && g > (0.-accuracy) 
            
                    -> molecule::acc


                |(g,c) when g > accuracy && c = 9 
                
                    -> inner allAtoms nodeAtoms (gap-x.mass) accuracy (x::molecule) acc x.valence
            
                |(g,c) when g > accuracy && c > 1 && c < 9
            
                    -> inner allAtoms nodeAtoms (gap-x.mass) accuracy (x::molecule) acc (counter-1)

                |(g,c) when g > accuracy && c = 1 
            
                    -> inner nodeAtoms nodeAtoms (gap-x.mass) accuracy (x::molecule) acc (x.valence-1) 

                |_
            
                    -> acc
            ) |> List.concat

        inner allAtoms nodeAtoms gap accuracy [] [] 9

