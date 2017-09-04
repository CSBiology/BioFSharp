namespace BioFSharp

open FSharp.Care

///Contains the AminoAcid type and its according functions. The AminoAcid type is a complex presentation of amino acids, allowing modifications
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
                ///Returns the one letter code of the AminoAcid as a char
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
                ///Returns the full formula of the AminoAcid and its modifications
                member this.Formula  =                                                         
                    let rec formula (aa:AminoAcid) =
                            match aa with
                            | AminoAcid.Ala -> Formula.Table.Ala 
                            | AminoAcid.Cys -> Formula.Table.Cys 
                            | AminoAcid.Asp -> Formula.Table.Asp 
                            | AminoAcid.Glu -> Formula.Table.Glu 
                            | AminoAcid.Phe -> Formula.Table.Phe 
                            | AminoAcid.Gly -> Formula.Table.Gly 
                            | AminoAcid.His -> Formula.Table.His 
                            | AminoAcid.Ile -> Formula.Table.Ile 
                            | AminoAcid.Lys -> Formula.Table.Lys 
                            | AminoAcid.Leu -> Formula.Table.Leu 
                            | AminoAcid.Met -> Formula.Table.Met 
                            | AminoAcid.Asn -> Formula.Table.Asn 
                            | AminoAcid.Pyl -> Formula.Table.Pyl // Pyrrolysine
                            | AminoAcid.Pro -> Formula.Table.Pro 
                            | AminoAcid.Gln -> Formula.Table.Gln 
                            | AminoAcid.Arg -> Formula.Table.Arg 
                            | AminoAcid.Ser -> Formula.Table.Ser 
                            | AminoAcid.Thr -> Formula.Table.Thr 
                            | AminoAcid.Sel -> Formula.Table.Sel // Selenocysteine
                            | AminoAcid.Val -> Formula.Table.Val 
                            | AminoAcid.Trp -> Formula.Table.Trp 
                            | AminoAcid.Tyr -> Formula.Table.Tyr 
                                                                
                            | AminoAcid.Xaa -> Formula.Table.Xaa  // Averagine Model -> C4.9384 H7.7583 N1.3577 O1.4773 S0.0417
                            | AminoAcid.Xle -> Formula.Table.Xle 
                            | AminoAcid.Glx -> Formula.Table.Glx 
                            | AminoAcid.Asx -> Formula.Table.Asx 
           
                            | AminoAcid.Gap -> (Formula.emptyFormula)
                            | AminoAcid.Ter -> (Formula.emptyFormula)

                            | AminoAcid.Mod (aa,mds) -> Seq.fold (fun acc (md:ModificationInfo.Modification) -> md.Modify acc ) (formula aa) mds
                    
                    formula this
                ///Returns true if the AminoAcid is a Terminator, otherwise returns false
                member this.isTerminator = match this with
                                           | AminoAcid.Ter -> true
                                           | _             -> false
                ///Returns true if the AminoAcid is a Gap, otherwise returns false
                member this.isGap        = match this with
                                           | AminoAcid.Gap -> true
                                           | _             -> false

                ///Returns the name of the AminoAcid and its modifications as a string
                member this.Name = 

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
                    
                    name this
        
        //static member op_Explicit (value:#IBioItem) = value.Symbol |> byte |> AminoAcidSymbols.aminoAcidSymbol
        static member op_Explicit (value:#IBioItem) : byte = byte value.Symbol
        static member op_Explicit (value:#IBioItem) : int = int value.Symbol


    // Sets amino acid modification 
    let setModification (md:ModificationInfo.Modification) (aa:AminoAcid) =
        match aa with
        | Mod (a,mds) -> Mod ( a ,  md::mds )
        | _           -> Mod ( aa, [md] )

    // Sets a multiple amino acid modifications 
    let setModifications (md:ModificationInfo.Modification list) (aa:AminoAcid) =
        match aa with
        | Mod (a,mds) -> Mod ( a ,  md@mds )
        | _           -> Mod ( aa, md )

    // Gets amino acid modification 
    let getModifications (aa:AminoAcid) =
        match aa with
        | Mod (_,mds) -> mds
        | _           -> []

    // Gets amino acid without the modification 
    let getAminoAcidWithoutMod (aa:AminoAcid) =
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
        | Mod (_,mds) -> 
            let tmpFn =
                 List.filter (fun (a:ModificationInfo.Modification) -> a.Location = ModificationInfo.ModLocation.Isotopic) mds                                
            match tmpFn with
            | [] -> f
            | l  -> 
                let fn = 
                    l
                    |> List.map (fun a -> a.Modify) 
                    |> List.reduce (>>) 
                fn f
        | _           -> f
                

    ///Lexer tags for parsing AminoAcids
    type ParsedAminoAcidChar = 
        | StandardCodes  of AminoAcid
        | AmbiguityCodes of AminoAcid
        | GapTer         of AminoAcid
        | NoAAChar       of char

    ///Simple Lexer for parsing AminoAcids from chars. The full parser is located in the BioItemsConverter-module
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

    ///Set of the 20 standard amino acids
    let AminoAcidSetStandard =
        set [
            AminoAcid.Ala
            AminoAcid.Cys
            AminoAcid.Asp
            AminoAcid.Glu
            AminoAcid.Phe
            AminoAcid.Gly
            AminoAcid.His
            AminoAcid.Ile
            AminoAcid.Lys
            AminoAcid.Leu
            AminoAcid.Met
            AminoAcid.Ala
            AminoAcid.Asn
            AminoAcid.Pyl
            AminoAcid.Pro
            AminoAcid.Gln
            AminoAcid.Arg
            AminoAcid.Ser
            AminoAcid.Thr
            AminoAcid.Sel
            AminoAcid.Val
            AminoAcid.Trp
            AminoAcid.Tyr ]  
    
    ///Set of all ambiguous codes
    let AminoAcidSetAmbiguity =
        set [
            AminoAcid.Xaa
            AminoAcid.Xle
            AminoAcid.Glx
            AminoAcid.Asx ]  
    
    ///Set containing the Gap and the Terminator AminoAcid
    let AminoAcidSetGapTer =
        set [
            AminoAcid.Gap
            AminoAcid.Ter ]

    ///Set of all AminoAcids with basic sidechain
    let AminoAcidSetPosCharged =
            set [
                AminoAcid.Arg
                AminoAcid.Lys
                AminoAcid.His ]
    
    ///Set of all AminoAcids with acidic sidechain
    let AminoAcidSetNegCharged =
            set [
                AminoAcid.Asp
                AminoAcid.Glu
                AminoAcid.Cys
                AminoAcid.Tyr ]
 
    /// Returns the name of AminoAcid
    let name (aa:AminoAcid) =
        BioItem.name aa

    //Returns amino acid formulas minus H20            
    let formula (aa:AminoAcid) =
        BioItem.formula aa
    
    /// Returns the symbol of AminoAcid       
    let symbol (aa:AminoAcid) =
        BioItem.symbol aa

    /// Returns true if AminoAcid represents a sequence terminator
    let isTerminator (aa:AminoAcid) =
        BioItem.isTerminator aa

    /// Returns true if AminoAcid represents a sequence gap
    let isGap (aa:AminoAcid) =
        BioItem.isGap aa

    /// Returns the monoisotopic mass of AminoAcid (without H20)
    let monoisoMass (aa:AminoAcid) =
        BioItem.monoisoMass aa

    /// Returns the average mass of AminoAcid (without H20)
    let averageMass (aa:AminoAcid) =
        BioItem.averageMass aa

    /// Returns a function to calculate the monoisotopic mass of a AminoAcid with memoization
    let initMonoisoMassWithMemP =
        Memoization.memoizeP (fun a -> monoisoMass a)          

    /// Returns a function to calculate the average mass of a AminoAcid with memoization
    let initAverageMassWithMemP = 
        Memoization.memoizeP (fun a -> averageMass a)
    
    /// Returns true, if the AminoAcid has a basic or acidic side chain
    let isCharged (aa:AminoAcid) =
        AminoAcidSetPosCharged.Contains aa || AminoAcidSetNegCharged.Contains aa 

    /// Returns true, if the AminoAcid has a basic side chain
    let isPosCharged (aa:AminoAcid) =
        AminoAcidSetPosCharged.Contains aa

    /// Returns true, if the AminoAcid has an acidic side chain
    let isNegCharged (aa:AminoAcid) =
        AminoAcidSetNegCharged.Contains aa 
    
        