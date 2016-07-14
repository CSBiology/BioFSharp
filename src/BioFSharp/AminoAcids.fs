namespace BioFSharp

open FSharp.Care

module AminoAcids =

    /// Amino acid Codes
    [<StructuralEquality;StructuralComparison>]
    type AminoAcid =
        /// A  *Alanin
        | Ala        
        /// C  *Cysteine
        | Cys
        /// D  *Aspartic Acid
        | Asp
        /// E  *Glutamic Acid
        | Glu 
        /// F  *Glutamic Acid
        | Phe
        /// G  *Glycine
        | Gly
        /// H  *Histidine
        | His
        /// I  *Isoleucine
        | Ile
        /// K  *Lysine
        | Lys
        /// L  *Leucine
        | Leu
        /// M  *Methionine
        | Met
        /// N  *Asparagine
        | Asn
        /// O  *Pyrrolysine
        | Pyl
        /// P  *Proline
        | Pro        
        /// Q  *Glutamine
        | Gln
        /// R  *Arginine
        | Arg
        /// S  *Serine
        | Ser        
        /// T  *Threonine
        | Thr
        /// U  *Selenocysteine
        | Sel
        /// V  *Valine
        | Val
        /// W  *Tryptophan
        | Trp
        /// Y  *Tyrosine
        | Tyr

        /// X  *Unspecified
        | Xaa        
        /// J  *Leucine/Isoleucine
        | Xle
        /// Z  *Glutamine/glutamic acid
        | Glx
        /// B  *Asparagine/aspartic acid
        | Asx

        /// -  *Gap 
        | Gap
        /// *  *Termination
        | Ter

        | Mod of AminoAcid * ModificationInfo.Modification list
        
        interface IBioItem with            
                member this.Symbol   = 
                    let rec symbol (aa:AminoAcid) =
                            match aa with
                            | AminoAcid.Ala -> 'A' 
                            | AminoAcid.Cys -> 'C'
                            | AminoAcid.Asp -> 'D'
                            | AminoAcid.Glu -> 'E'
                            | AminoAcid.Phe -> 'F'
                            | AminoAcid.Gly -> 'G'
                            | AminoAcid.His -> 'H'
                            | AminoAcid.Ile -> 'I'
                            | AminoAcid.Lys -> 'K'
                            | AminoAcid.Leu -> 'L'
                            | AminoAcid.Met -> 'M'
                            | AminoAcid.Asn -> 'N'
                            | AminoAcid.Pyl -> 'O'
                            | AminoAcid.Pro -> 'P'
                            | AminoAcid.Gln -> 'Q'
                            | AminoAcid.Arg -> 'R'
                            | AminoAcid.Ser -> 'S'
                            | AminoAcid.Thr -> 'T'
                            | AminoAcid.Sel -> 'U'
                            | AminoAcid.Val -> 'V'
                            | AminoAcid.Trp -> 'W'
                            | AminoAcid.Tyr -> 'Y'
              
                            | AminoAcid.Xaa -> 'X'
                            | AminoAcid.Xle -> 'J'
                            | AminoAcid.Glx -> 'Z'
                            | AminoAcid.Asx -> 'B'
              
                            | AminoAcid.Gap -> '-'
                            | AminoAcid.Ter -> '*'

                            | AminoAcid.Mod (aa,_) -> (symbol aa) |> System.Char.ToLower
                    symbol this
                
                member this.Formula  = 
                    //Amino acid formulas minus H20   
                    let rec formula (aa:AminoAcid) =
                            match aa with
                            | AminoAcid.Ala -> (Formula.parseFormulaString "C3H5ON"    )
                            | AminoAcid.Cys -> (Formula.parseFormulaString "C3H5ONS"   )
                            | AminoAcid.Asp -> (Formula.parseFormulaString "C4H5O3N"   )
                            | AminoAcid.Glu -> (Formula.parseFormulaString "C5H7O3N"   )
                            | AminoAcid.Phe -> (Formula.parseFormulaString "C9H9ON"    )
                            | AminoAcid.Gly -> (Formula.parseFormulaString "C2H3ON"    )
                            | AminoAcid.His -> (Formula.parseFormulaString "C6H7ON3"   )
                            | AminoAcid.Ile -> (Formula.parseFormulaString "C6H11ON"   )
                            | AminoAcid.Lys -> (Formula.parseFormulaString "C6H12ON2"  )
                            | AminoAcid.Leu -> (Formula.parseFormulaString "C6H11ON"   )
                            | AminoAcid.Met -> (Formula.parseFormulaString "C5H9ONS"   )
                            | AminoAcid.Asn -> (Formula.parseFormulaString "C4H6O2N2"  )
                            | AminoAcid.Pyl -> (Formula.parseFormulaString "C12H19N3O2") // Pyrrolysine
                            | AminoAcid.Pro -> (Formula.parseFormulaString "C5H7ON"    )
                            | AminoAcid.Gln -> (Formula.parseFormulaString "C5H8O2N2"  )
                            | AminoAcid.Arg -> (Formula.parseFormulaString "C6H12ON4"  )
                            | AminoAcid.Ser -> (Formula.parseFormulaString "C3H5O2N"   )
                            | AminoAcid.Thr -> (Formula.parseFormulaString "C4H7O2N"   )
                            | AminoAcid.Sel -> (Formula.parseFormulaString "C3H5NOSe"  ) // Selenocysteine
                            | AminoAcid.Val -> (Formula.parseFormulaString "C5H9ON"    )
                            | AminoAcid.Trp -> (Formula.parseFormulaString "C11H10ON2" )
                            | AminoAcid.Tyr -> (Formula.parseFormulaString "C9H9O2N"   )
           
                            | AminoAcid.Xaa -> (Formula.parseFormulaString "C2H3N1O1"  )  // Averagine Model -> C4.9384 H7.7583 N1.3577 O1.4773 S0.0417
                            | AminoAcid.Xle -> (Formula.parseFormulaString "C6H11N1O1" )
                            | AminoAcid.Glx -> (Formula.parseFormulaString "C5H7N1O3"  )
                            | AminoAcid.Asx -> (Formula.parseFormulaString "C4H5N1O3"  )
           
                            | AminoAcid.Gap -> (Formula.emptyFormula)
                            | AminoAcid.Ter -> (Formula.emptyFormula)

                            | AminoAcid.Mod (aa,mds) -> Seq.fold (fun acc (md:ModificationInfo.Modification) -> md.Modify acc ) (formula aa) mds
                    
                    formula this
                
                member this.isTerminator = match this with
                                           | AminoAcid.Ter -> true
                                           | _             -> false
                member this.isGap        = match this with
                                           | AminoAcid.Gap -> true
                                           | _             -> false

    // Sets amino acid modification 
    let setModification (md:ModificationInfo.Modification) (aa:AminoAcid) =
        match aa with
        | Mod (a,mds) -> Mod ( a ,  md::mds )
        | _           -> Mod ( aa, [md] )

    // Gets amino acid modification 
    let getModifications (aa:AminoAcid) =
        match aa with
        | Mod (_,mds) -> mds
        | _           -> []


    // Gets amino acid modification 
    let tryGetModifications (aa:AminoAcid) =
        match aa with
        | Mod (_,mds) -> Some mds
        | _           -> None



    let isotopicLabelFunc (aa:AminoAcid) (f:Formula.Formula)  = 
        match aa with
        | Mod (_,mds) -> let fn =
                            mds
                            |> List.filter (fun a -> a.Location = ModificationInfo.ModLocation.Isotopic)
                            |> List.map (fun a -> a.Modify)
                            |> List.reduce (>>)
                         fn f
        | _           -> f
                

    
    type ParsedAminoAcidChar = 
        | StandardCodes  of AminoAcid
        | AmbiguityCodes of AminoAcid
        | GapTer         of AminoAcid
        | NoAAChar       of char


    let charToParsedAminoAcidChar (c:char) =
        match System.Char.ToUpper c with                                    
        | 'A' ->  StandardCodes AminoAcid.Ala            
        | 'C' ->  StandardCodes AminoAcid.Cys
        | 'D' ->  StandardCodes AminoAcid.Asp
        | 'E' ->  StandardCodes AminoAcid.Glu
        | 'F' ->  StandardCodes AminoAcid.Phe            
        | 'G' ->  StandardCodes AminoAcid.Gly
        | 'H' ->  StandardCodes AminoAcid.His
        | 'I' ->  StandardCodes AminoAcid.Ile            
        | 'K' ->  StandardCodes AminoAcid.Lys
        | 'L' ->  StandardCodes AminoAcid.Leu            
        | 'M' ->  StandardCodes AminoAcid.Met
        | 'N' ->  StandardCodes AminoAcid.Asn            
        | 'P' ->  StandardCodes AminoAcid.Pro
        | 'Q' ->  StandardCodes AminoAcid.Gln
        | 'R' ->  StandardCodes AminoAcid.Arg            
        | 'S' ->  StandardCodes AminoAcid.Ser
        | 'T' ->  StandardCodes AminoAcid.Thr            
        | 'V' ->  StandardCodes AminoAcid.Val
        | 'W' ->  StandardCodes AminoAcid.Trp
        | 'Y' ->  StandardCodes AminoAcid.Tyr
        // special amino acids
        | 'O' ->  StandardCodes AminoAcid.Pyl
        | 'U' ->  StandardCodes AminoAcid.Sel
        // ambiguis amino acids
        | 'X' ->  AmbiguityCodes AminoAcid.Xaa            
        | 'Z' ->  AmbiguityCodes AminoAcid.Glx
        | 'B' ->  AmbiguityCodes AminoAcid.Asx
        | 'J' ->  AmbiguityCodes AminoAcid.Xle
        // termination and gap
        | '-' ->  GapTer AminoAcid.Gap
        | '*' ->  GapTer AminoAcid.Ter            
        // no amino acid character
        | ch -> NoAAChar ch



//  ###############------------------------------------------------------------

           
    /// Returns the name of AminoAcid
    let rec name (aa:AminoAcid) =
            match aa with
            | AminoAcid.Ala -> "Alanin"          
            | AminoAcid.Cys -> "Cysteine"       
            | AminoAcid.Asp -> "Aspartic Acid"  
            | AminoAcid.Glu -> "Glutamic Acid"  
            | AminoAcid.Phe -> "Phenylalanin"   
            | AminoAcid.Gly -> "Glycine"        
            | AminoAcid.His -> "Histidine"      
            | AminoAcid.Ile -> "Isoleucine"     
            | AminoAcid.Lys -> "Lysine"         
            | AminoAcid.Leu -> "Leucine"        
            | AminoAcid.Met -> "Methionine"     
            | AminoAcid.Asn -> "Asparagine"     
            | AminoAcid.Pyl -> "Pyrrolysine"    
            | AminoAcid.Pro -> "Proline"        
            | AminoAcid.Gln -> "Glutamine"      
            | AminoAcid.Arg -> "Arginine"       
            | AminoAcid.Ser -> "Serine"         
            | AminoAcid.Thr -> "Threonine"      
            | AminoAcid.Sel -> "Selenocysteine" 
            | AminoAcid.Val -> "Valine"         
            | AminoAcid.Trp -> "Tryptophan"     
            | AminoAcid.Tyr -> "Tyrosine"       
             
            | AminoAcid.Xaa -> "Unspecified"             
            | AminoAcid.Xle -> "Leucine/Isoleucine"      
            | AminoAcid.Glx -> "Glutamine/glutamic acid" 
            | AminoAcid.Asx -> "Asparagine/aspartic acid"
             
            | AminoAcid.Gap -> "Gap"
            | AminoAcid.Ter -> "Ter"

            | AminoAcid.Mod (aa,mds) -> sprintf "%s[%s]" (name aa) (mds |> Seq.map (fun md -> md.Name) |> String.concat ";")

    //Returns amino acid formulas minus H20            
    let formula (aa:AminoAcid) =
        BioItem.formula aa

//    //Returns amino acid formulas minus H20            
//    let formulaOfModificationOnly (aa:AminoAcid) =
//        match aa with
//        | AminoAcid.Mod (aa,mds) -> 
//        | _ -> Formula.emptyFormula
    
    /// Returns the symbol of AminoAcid       
    let symbol (aa:AminoAcid) =
        BioItem.symbol aa
    
    
    /// Returns the monoisotopic mass of AminoAcid (without H20)
    let monoisoMass =
        let monoisoMass' = formula >> Formula.monoisoMass
        Memoization.memoizeP (fun a -> monoisoMass' a)
            

    /// Returns the average mass of AminoAcid (without H20)
    let averageMass = 
        let averageMass' = formula >> Formula.averageMass
        Memoization.memoizeP (fun a -> averageMass' a)

