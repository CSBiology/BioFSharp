namespace BioFSharp

open FSharp.Care

///Contains the AminoAcidSymbol type and its according functions. The AminoAcidSymbol type is a lightweight, efficient presentation of amino acids
module AminoAcidSymbols =

    // Partial active pattern. Match if field equals value.
    let (|Field|_|) field x = if field = x then Some () else None

    [<Struct>]
    [<CustomEquality;CustomComparison>]
    /// Symbols of all amino acids (including Gap + Term) 
    type AminoAcidSymbol internal(value:byte) =    
        member private this.Value = value
        /// A  *Alanin
        static member Ala = AminoAcidSymbol (byte 'A')
        /// C  *Cysteine
        static member Cys = AminoAcidSymbol (byte 'C')
        /// D  *Aspartic Acid
        static member Asp = AminoAcidSymbol (byte 'D')
        /// E  *Glutamic Acid
        static member Glu = AminoAcidSymbol (byte 'E')
        /// F  *Glutamic Acid
        static member Phe = AminoAcidSymbol (byte 'F')
        /// G  *Glycine
        static member Gly = AminoAcidSymbol (byte 'G')
        /// H  *Histidine
        static member His = AminoAcidSymbol (byte 'H')
        /// I  *Isoleucine
        static member Ile = AminoAcidSymbol (byte 'I')
        /// K  *Lysine
        static member Lys = AminoAcidSymbol (byte 'K')
        /// L  *Leucine
        static member Leu = AminoAcidSymbol (byte 'L')
        /// M  *Methionine
        static member Met = AminoAcidSymbol (byte 'M')
        /// N  *Asparagine
        static member Asn = AminoAcidSymbol (byte 'N')
        /// P  *Proline
        static member Pro = AminoAcidSymbol (byte 'P')
        /// Q  *Glutamine
        static member Gln = AminoAcidSymbol (byte 'Q')
        /// R  *Arginine
        static member Arg = AminoAcidSymbol (byte 'R')
        /// S  *Serine
        static member Ser = AminoAcidSymbol (byte 'S')
        /// T  *Threonine
        static member Thr = AminoAcidSymbol (byte 'T')
        /// V  *Valine
        static member Val = AminoAcidSymbol (byte 'V')
        /// W  *Tryptophan
        static member Trp = AminoAcidSymbol (byte 'W')
        /// Y  *Tyrosine
        static member Tyr = AminoAcidSymbol (byte 'Y')
    
        /// O  *Pyrrolysine   
        static member Pyl = AminoAcidSymbol (byte 'O')
        /// U  *Selenocysteine
        static member Sel = AminoAcidSymbol (byte 'U')
                                               
        /// X  *Unspecified
        static member Xaa = AminoAcidSymbol (byte 'X')
        /// J  *Leucine/Isoleucine
        static member Xle = AminoAcidSymbol (byte 'J')
        /// Z  *Glutamine/glutamic acid
        static member Glx = AminoAcidSymbol (byte 'Z')
        /// B  *Asparagine/aspartic acid
        static member Asx = AminoAcidSymbol (byte 'B')
                                           
        /// -  *Gap 
        static member Gap = AminoAcidSymbol (byte '-')
        /// *  *Termination
        static member Ter = AminoAcidSymbol (byte '*')

        static member op_Explicit (value) = AminoAcidSymbol(byte value)
        static member op_Explicit (value:AminoAcidSymbol) : byte = value.Value
        static member op_Explicit (value:AminoAcidSymbol) : int = int value.Value

        override this.Equals(other) =
            match other with
                | :? AminoAcidSymbol as o -> this.Value = o.Value
                | _ -> false

        ///Returns integer hashcode of AminoAcidSymbol
        override this.GetHashCode () = hash value

        ///Returns one letter code of AminoAcidSymbol as string
        override this.ToString () = sprintf "%c" ((char)value)


        // #region System.IComparable
        interface System.IComparable with

          member this.CompareTo other =
              match other with
              | :? AminoAcidSymbol as o ->  compare this.Value o.Value //if (this.Equals(other)) then 1 else 0
              | _ -> invalidArg "other" "cannot compare values of different types"
        // #end region System.IComparable        

        interface IBioItem with
            ///Returns one letter code of AminoAcidSymbol as char
            member this.Symbol = (char)value     
            ///Returns true, if AminoAcidSymbol is Terminator, otherwise returns false
            member this.isTerminator = this.Value = AminoAcidSymbol.Ter.Value
            ///Returns true, if AminoAcidSymbol is Gap, otherwise returns false
            member this.isGap        = this.Value = AminoAcidSymbol.Gap.Value
            //Amino acid formulas minus H20  
            member this.Formula  = 
                let formula' (aa:AminoAcidSymbol) =
                    printfn "outer"
                    match aa with                    
                    | x when x = AminoAcidSymbol.Ala -> 
                        printfn "Formula.Table.Ala"
                        Map.empty
                        //Formula.emptyFormula //Formula.Table.Ala
                    | _ -> failwithf "Alarm"
//                    | Field AminoAcidSymbol.Cys -> Formula.Table.Cys
//                    | Field AminoAcidSymbol.Asp -> Formula.Table.Asp
//                    | Field AminoAcidSymbol.Glu -> Formula.Table.Glu
//                    | Field AminoAcidSymbol.Phe -> Formula.Table.Phe
//                    | Field AminoAcidSymbol.Gly -> Formula.Table.Gly
//                    | Field AminoAcidSymbol.His -> Formula.Table.His
//                    | Field AminoAcidSymbol.Ile -> Formula.Table.Ile
//                    | Field AminoAcidSymbol.Lys -> Formula.Table.Lys
//                    | Field AminoAcidSymbol.Leu -> Formula.Table.Leu
//                    | Field AminoAcidSymbol.Met -> Formula.Table.Met
//                    | Field AminoAcidSymbol.Asn -> Formula.Table.Asn
//                    | Field AminoAcidSymbol.Pyl -> Formula.Table.Pyl // Pyrrolysine
//                    | Field AminoAcidSymbol.Pro -> Formula.Table.Pro
//                    | Field AminoAcidSymbol.Gln -> Formula.Table.Gln
//                    | Field AminoAcidSymbol.Arg -> Formula.Table.Arg
//                    | Field AminoAcidSymbol.Ser -> Formula.Table.Ser
//                    | Field AminoAcidSymbol.Thr -> Formula.Table.Thr
//                    | Field AminoAcidSymbol.Sel -> Formula.Table.Sel // Selenocysteine
//                    | Field AminoAcidSymbol.Val -> Formula.Table.Val
//                    | Field AminoAcidSymbol.Trp -> Formula.Table.Trp
//                    | Field AminoAcidSymbol.Tyr -> Formula.Table.Tyr
//                                                                    
//                    | Field AminoAcidSymbol.Xaa -> Formula.Table.Xaa  // Averagine Model -> C4.9384 H7.7583 N1.3577 O1.4773 S0.0417
//                    | Field AminoAcidSymbol.Xle -> Formula.Table.Xle
//                    | Field AminoAcidSymbol.Glx -> Formula.Table.Glx
//                    | Field AminoAcidSymbol.Asx -> Formula.Table.Asx
//                                               
//                    | Field AminoAcidSymbol.Gap -> Formula.emptyFormula
//                    | Field AminoAcidSymbol.Ter -> Formula.emptyFormula
//                    | _                         -> Formula.emptyFormula

                formula' AminoAcidSymbol.Ala 

            /// Returns the name of AminoAcidSymbol as string
            member this.Name =                   
                
                match this with
                | Field AminoAcidSymbol.Ala -> "Alanin"          
                | Field AminoAcidSymbol.Cys -> "Cysteine"       
                | Field AminoAcidSymbol.Asp -> "Aspartic Acid"  
                | Field AminoAcidSymbol.Glu -> "Glutamic Acid"  
                | Field AminoAcidSymbol.Phe -> "Phenylalanin"   
                | Field AminoAcidSymbol.Gly -> "Glycine"        
                | Field AminoAcidSymbol.His -> "Histidine"      
                | Field AminoAcidSymbol.Ile -> "Isoleucine"     
                | Field AminoAcidSymbol.Lys -> "Lysine"         
                | Field AminoAcidSymbol.Leu -> "Leucine"        
                | Field AminoAcidSymbol.Met -> "Methionine"     
                | Field AminoAcidSymbol.Asn -> "Asparagine"     
                | Field AminoAcidSymbol.Pyl -> "Pyrrolysine"    
                | Field AminoAcidSymbol.Pro -> "Proline"        
                | Field AminoAcidSymbol.Gln -> "Glutamine"      
                | Field AminoAcidSymbol.Arg -> "Arginine"       
                | Field AminoAcidSymbol.Ser -> "Serine"         
                | Field AminoAcidSymbol.Thr -> "Threonine"      
                | Field AminoAcidSymbol.Sel -> "Selenocysteine" 
                | Field AminoAcidSymbol.Val -> "Valine"         
                | Field AminoAcidSymbol.Trp -> "Tryptophan"     
                | Field AminoAcidSymbol.Tyr -> "Tyrosine"       
             
                | Field AminoAcidSymbol.Xaa -> "Unspecified"             
                | Field AminoAcidSymbol.Xle -> "Leucine/Isoleucine"      
                | Field AminoAcidSymbol.Glx -> "Glutamine/glutamic acid" 
                | Field AminoAcidSymbol.Asx -> "Asparagine/aspartic acid"
                                       
                | Field AminoAcidSymbol.Gap -> "Gap"
                | Field AminoAcidSymbol.Ter -> "Ter"
                                     
                | _ -> failwithf "Not an amino acid symbol"
                
    /// Maps input to AminoAcidSymbol if possible
    let inline aminoAcidSymbol a = AminoAcidSymbol.op_Explicit(int a)

    let inline parseChar (c:char) =
        match System.Char.ToUpper c with                                    
        | 'A' ->  NcbiParsingType.StandardCodes, Some AminoAcidSymbol.Ala            
        | 'C' ->  NcbiParsingType.StandardCodes, Some AminoAcidSymbol.Cys
        | 'D' ->  NcbiParsingType.StandardCodes, Some AminoAcidSymbol.Asp
        | 'E' ->  NcbiParsingType.StandardCodes, Some AminoAcidSymbol.Glu
        | 'F' ->  NcbiParsingType.StandardCodes, Some AminoAcidSymbol.Phe            
        | 'G' ->  NcbiParsingType.StandardCodes, Some AminoAcidSymbol.Gly
        | 'H' ->  NcbiParsingType.StandardCodes, Some AminoAcidSymbol.His
        | 'I' ->  NcbiParsingType.StandardCodes, Some AminoAcidSymbol.Ile            
        | 'K' ->  NcbiParsingType.StandardCodes, Some AminoAcidSymbol.Lys
        | 'L' ->  NcbiParsingType.StandardCodes, Some AminoAcidSymbol.Leu            
        | 'M' ->  NcbiParsingType.StandardCodes, Some AminoAcidSymbol.Met
        | 'N' ->  NcbiParsingType.StandardCodes, Some AminoAcidSymbol.Asn            
        | 'P' ->  NcbiParsingType.StandardCodes, Some AminoAcidSymbol.Pro
        | 'Q' ->  NcbiParsingType.StandardCodes, Some AminoAcidSymbol.Gln
        | 'R' ->  NcbiParsingType.StandardCodes, Some AminoAcidSymbol.Arg            
        | 'S' ->  NcbiParsingType.StandardCodes, Some AminoAcidSymbol.Ser
        | 'T' ->  NcbiParsingType.StandardCodes, Some AminoAcidSymbol.Thr            
        | 'V' ->  NcbiParsingType.StandardCodes, Some AminoAcidSymbol.Val
        | 'W' ->  NcbiParsingType.StandardCodes, Some AminoAcidSymbol.Trp
        | 'Y' ->  NcbiParsingType.StandardCodes, Some AminoAcidSymbol.Tyr
        // special amino acids
        | 'O' ->  NcbiParsingType.StandardCodes, Some AminoAcidSymbol.Pyl
        | 'U' ->  NcbiParsingType.StandardCodes, Some AminoAcidSymbol.Sel
        // ambiguis amino acids
        | 'X' ->  NcbiParsingType.AmbiguityCodes, Some AminoAcidSymbol.Xaa            
        | 'Z' ->  NcbiParsingType.AmbiguityCodes, Some AminoAcidSymbol.Glx
        | 'B' ->  NcbiParsingType.AmbiguityCodes, Some AminoAcidSymbol.Asx
        | 'J' ->  NcbiParsingType.AmbiguityCodes, Some AminoAcidSymbol.Xle
        // termination and gap
        | '-' ->  NcbiParsingType.GapTer, Some AminoAcidSymbol.Gap
        | '*' ->  NcbiParsingType.GapTer, Some AminoAcidSymbol.Ter            
        // no amino acid character
        | _  -> NcbiParsingType.NoAAChar, None

    ///Set of the 20 standard amino acids
    let AminoSymbolSetStandard =
        set [
            AminoAcidSymbol.Ala
            AminoAcidSymbol.Cys
            AminoAcidSymbol.Asp
            AminoAcidSymbol.Glu
            AminoAcidSymbol.Phe
            AminoAcidSymbol.Gly
            AminoAcidSymbol.His
            AminoAcidSymbol.Ile
            AminoAcidSymbol.Lys
            AminoAcidSymbol.Leu
            AminoAcidSymbol.Met
            AminoAcidSymbol.Ala
            AminoAcidSymbol.Asn
            AminoAcidSymbol.Pyl
            AminoAcidSymbol.Pro
            AminoAcidSymbol.Gln
            AminoAcidSymbol.Arg
            AminoAcidSymbol.Ser
            AminoAcidSymbol.Thr
            AminoAcidSymbol.Sel
            AminoAcidSymbol.Val
            AminoAcidSymbol.Trp
            AminoAcidSymbol.Tyr ]  
    
    let AminoSymbolSetAmbiguity =
        set [
            AminoAcidSymbol.Xaa
            AminoAcidSymbol.Xle
            AminoAcidSymbol.Glx
            AminoAcidSymbol.Asx ]  
    
    ///Set containing the Gap and the Terminator AminoAcidSymbols
    let AminoSymbolSetGapTer =
        set [
            AminoAcidSymbol.Gap
            AminoAcidSymbol.Ter ] 

    ///Set of all AminoAcidSymbols with basic sidechain
    let AminoSymbolSetPosCharged =
        set [
            AminoAcidSymbol.Arg
            AminoAcidSymbol.Lys
            AminoAcidSymbol.His ]

    ///Set of all AminoAcidSymbols with acidic sidechain
    let AminoSymbolSetNegCharged =
        set [

            AminoAcidSymbol.Asp
            AminoAcidSymbol.Glu
            AminoAcidSymbol.Cys
            AminoAcidSymbol.Tyr ]

    /// Returns the name of AminoAcidSymbol
    let name (aa:AminoAcidSymbol) =
        BioItem.name aa

    ///Returns AminoAcidSymbol formulas minus H20            
    let formula (aa:AminoAcidSymbol) =        
        BioItem.formula aa
    
    /// Returns the symbol of AminoAcidSymbol       
    let symbol (aa:AminoAcidSymbol) =
        BioItem.symbol aa

    /// Returns true if AminoAcidSymbol represents a sequence terminator
    let isTerminator (aa:AminoAcidSymbol) =
        BioItem.isTerminator aa

    /// Returns true if AminoAcidSymbol represents a sequence gap
    let isGap (aa:AminoAcidSymbol) =
        BioItem.isGap aa

    /// Returns the monoisotopic mass of AminoAcidSymbol (without H20)
    let monoisoMass (aa:AminoAcidSymbol) =
        BioItem.monoisoMass aa

    /// Returns the average mass of AminoAcidSymbol (without H20)
    let averageMass (aa:AminoAcidSymbol) =
        BioItem.averageMass aa

    /// Returns a function to calculate the monoisotopic mass of a bio item with memoization
    let initMonoisoMassWithMemP =
        Memoization.memoizeP (fun a -> monoisoMass a)          

    /// Returns a function to calculate the average mass of a bio item with memoization
    let initAverageMassWithMemP = 
        Memoization.memoizeP (fun a -> averageMass a)

    /// Returns true, if the AminoAcidSymbol has a basic or acidic side chain
    let isCharged (aa:AminoAcidSymbol) = 
        AminoSymbolSetPosCharged.Contains aa || AminoSymbolSetNegCharged.Contains aa

    /// Returns true, if the AminoAcidSymbol has a basic side chain
    let isPosCharged (aa:AminoAcidSymbol) = 
        AminoSymbolSetPosCharged.Contains aa  
        
    /// Returns true, if the AminoAcidSymbol has an acidic side chain
    let isNegCharged (aa:AminoAcidSymbol) = 
        AminoSymbolSetNegCharged.Contains aa

