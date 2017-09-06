namespace BioFSharp

///Contains the Nucleotide type and its according functions.
module Nucleotides =
    
    open FSharp.Care
    open AminoAcids

    /// Nucleotide Codes
    type Nucleotide =
    // ´Standard Nucleotide Codes
    /// A : Adenine
    | A
    /// T : Thymidine (only DNA)
    | T
    /// G : Guanine
    | G
    /// C : Cytosine
    | C
    /// U : Uracil    (only RNA)
    | U
    /// I : Inosine   (only RNA)
    | I
    /// - : Gap
    | Gap
    /// * : Terminator
    | Ter
        
    // 'Ambiguous Nucleotide Codes: double base codes
    /// R : G or A = puRine
    | R
    /// Y : U/T or C = pYrimidine
    | Y
    /// K : G or U = Keto
    | K
    /// M : A or C = aMino
    | M
    /// S : G or C = Strong base pair
    | S
    /// W : A or U = Weak base pair 
    | W
        
    // 'Ambiguous Nucleotide Codes: triple base codes
    /// B : G or U or C = not A
    | B
    /// D : G or A or U = not C
    | D
    /// H : A or C or U = not G
    | H
    /// V : G or C or A = not T/U
    | V

    // 'Ambiguous Nucleotide Codes
    /// N : A or G or U or C.
    | N

        interface IBioItem with            
                
                ///Returns the one letter code of the nucleotide as a char
                member this.Symbol   = 
                    let rec symbol (nuc:Nucleotide) =
                            match nuc with
                            // ´Standard Nucleotide Codes
                            /// A : Adenine
                            | A   -> 'A'
                            /// T : Thymidine (only DNA)
                            | T   -> 'T'
                            /// G : Guanine
                            | G   -> 'G'
                            /// C : Cytosine
                            | C   -> 'C'
                            /// U : Uracil    (only RNA)
                            | U   -> 'U'
                            /// I : Inosine   (only RNA)
                            | I   -> 'I'
                            /// - : Gap
                            | Gap -> '-'
                            /// * : Terminator
                            | Ter -> '*'
        
                            // 'Ambiguous Nucleotide Codes: double base codes
                            /// R : G or A = puRine
                            | R   -> 'R'
                            /// Y : U/T or C = pYrimidine
                            | Y   -> 'Y'
                            /// K : G or U = Keto
                            | K   -> 'K'
                            /// M : A or C = aMino
                            | M   -> 'M'
                            /// S : G or C = Strong base pair
                            | S   -> 'S'
                            /// W : A or U = Weak base pair 
                            | W   -> 'W'
        
                            // 'Ambiguous Nucleotide Codes: triple base codes
                            /// B : G or U or C = not A
                            | B   -> 'B'
                            /// D : G or A or U = not C
                            | D   -> 'D'
                            /// H : A or C or U = not G
                            | H   -> 'H'
                            /// V : G or V or A = not T/U
                            | V   -> 'V'

                            // 'Ambiguous Nucleotide Codes
                            /// N : A or G or U or C.
                            | N   -> 'N' 
                    symbol this
                
                /// Returns the formula of the nucleotide
                member this.Formula  = 
                    //Amino acid formulas minus H20   
                    let rec formula (nuc:Nucleotide) =
                            
                        match nuc with
                        // ´Standard Nucleotide Codes
                        /// A : Adenine
                        | A   -> Formula.Table.A
                        /// T : Thymidine (only DNA)
                        | T   -> Formula.Table.T
                        /// G : Guanine
                        | G   -> Formula.Table.G
                        /// C : Cytosine
                        | C   -> Formula.Table.C
                        /// U : Uracil    (only RNA)
                        | U   -> Formula.Table.U
                        /// I : Inosine   (only RNA)
                        | I   -> Formula.Table.I
                        /// - : Gap
                        | Gap -> (Formula.emptyFormula)
                        /// * : Terminator
                        | Ter -> (Formula.emptyFormula)
        
                        // 'Ambiguous Nucleotide Codes: double base codes
                        /// R : G or A = puRine
                        | R   -> (Formula.emptyFormula)
                        /// Y : U/T or C = pYrimidine
                        | Y   -> (Formula.emptyFormula)
                        /// K : G or U = Keto
                        | K   -> (Formula.emptyFormula)
                        /// M : A or C = aMino
                        | M   -> (Formula.emptyFormula)
                        /// S : G or C = Strong base pair
                        | S   -> (Formula.emptyFormula)
                        /// W : A or U = Weak base pair 
                        | W   -> (Formula.emptyFormula)
        
                        // 'Ambiguous Nucleotide Codes: triple base codes
                        /// B : G or U or C = not A
                        | B   -> (Formula.emptyFormula)
                        /// D : G or A or U = not C
                        | D   -> (Formula.emptyFormula)
                        /// H : A or C or U = not G
                        | H   -> (Formula.emptyFormula)
                        /// V : G or V or A = not T/U
                        | V   -> (Formula.emptyFormula)

                        // 'Ambiguous Nucleotide Codes
                        /// N : A or G or U or C.
                        | N   -> (Formula.emptyFormula)
                    
                    formula this
                
                ///Returns true, if the nucleotide is a terminator, otherwise returns false
                member this.isTerminator = match this with
                                           | Nucleotide.Ter -> true
                                           | _             -> false
                ///Returns true, if the nucleotide is a gap, otherwise returns false
                member this.isGap        = match this with
                                           | Nucleotide.Gap -> true
                                           | _             -> false

                ///Returns the full name of the nucleotide as a string
                member this.Name = 
                    // Nucleotide names
                    let name (nuc:Nucleotide) =
                        match nuc with
                        // ´Standard Nucleotide Codes
                        /// A : Adenine
                        | A   -> "Adenine"
                        /// T : Thymidine (only DNA)
                        | T   -> "Thymidine"
                        /// G : Guanine
                        | G   -> "Guanine"
                        /// C : Cytosine
                        | C   -> "Cytosine"
                        /// U : Uracil    (only RNA)
                        | U   -> "Uracil"
                        /// I : Inosine   (only RNA)
                        | I   -> "Inosine"
                        /// - : Gap
                        | Gap -> "Gap"
                        /// * : Terminator
                        | Ter -> "Ter"
        
                        // 'Ambiguous Nucleotide Codes: double base codes
                        /// R : G or A = puRine
                        | R   ->  "puRine"
                        /// Y : U/T or C = pYrimidine
                        | Y   ->  "pYrimidine"
                        /// K : G or U = Keto
                        | K   ->  "Keto"
                        /// M : A or C = aMino
                        | M   -> "aMino"
                        /// S : G or C = Strong base pair
                        | S   ->  "Strong base pair"
                        /// W : A or U = Weak base pair 
                        | W   ->  "Weak base pair"
        
                        // 'Ambiguous Nucleotide Codes: triple base codes
                        /// B : G or U or C = not A
                        | B   ->  "not A"
                        /// D : G or A or U = not C
                        | D   ->  "not C"
                        /// H : A or C or U = not G
                        | H   ->  "not G"
                        /// V : G or V or A = not T/U
                        | V   ->  "not T/U"

                        // 'Ambiguous Nucleotide Codes
                        /// N : A or G or U or C.
                        | N   ->  "Unspecified"  
                        
                    name this               

        //static member op_Explicit (value) = (byte value)
        static member op_Explicit (value:#IBioItem) : byte = byte value.Symbol
        static member op_Explicit (value:#IBioItem) : int = int value.Symbol



    ///Lexer tags for parsing Nucleotides
    type ParsedNucleotideChar = 
        | StandardCodes    of Nucleotide
        | Standard_DNAonly of Nucleotide
        | Standard_RNAonly of Nucleotide
        | GapTer           of Nucleotide
        | AmbiguityCodes   of Nucleotide                
        | NoNucChar        of char

    ///Simple Lexer for parsing Nucleotides from chars. The full parser is located in the BioItemsConverter-module
    let charToParsedNucleotideChar (c:char) =
        match System.Char.ToUpper c with                                    
        
        | 'A' -> StandardCodes    Nucleotide.A
        | 'T' -> Standard_DNAonly Nucleotide.T
        | 'G' -> StandardCodes    Nucleotide.G
        | 'C' -> StandardCodes    Nucleotide.C
            
        | 'U' -> Standard_RNAonly Nucleotide.U
        | 'I' -> Standard_RNAonly Nucleotide.I
        // termination and gap
        | '-' -> GapTer           Nucleotide.Gap
        | '*' -> GapTer           Nucleotide.Ter

        | 'R' -> AmbiguityCodes Nucleotide.R
        | 'Y' -> AmbiguityCodes Nucleotide.Y
        | 'K' -> AmbiguityCodes Nucleotide.K
        | 'M' -> AmbiguityCodes Nucleotide.M
        | 'S' -> AmbiguityCodes Nucleotide.S
        | 'W' -> AmbiguityCodes Nucleotide.W

        | 'B' -> AmbiguityCodes Nucleotide.B
        | 'D' -> AmbiguityCodes Nucleotide.D
        | 'H' -> AmbiguityCodes Nucleotide.H
        | 'V' -> AmbiguityCodes Nucleotide.V

        | 'N' -> AmbiguityCodes Nucleotide.N        
        // bad character
        | ch -> NoNucChar ch
        
        
        
    /// Create the complement DNA or RNA strand. For example, the sequence "ATGC" is converted to "TACG"
    let complement (nuc:Nucleotide) =
        match nuc with
        | A    -> T 
        | T    -> A  
        | G    -> C
        | C    -> G
        | U    -> A                
        | R    -> Y
        | Y    -> R
        | K    -> M
        | M    -> K
        | B    -> V
        | D    -> H
        | H    -> D
        | V    -> B
        | _    -> nuc

        
    /// Create the inverse DNA or RNA strand. For example, the sequence "ATGC" is converted to "CGTA"
    let inverse (nuc:Nucleotide) =           
        match nuc with
        | A -> C
        | T -> G
        | G -> T 
        | C -> A
            
        | U -> A        
                        
        // 'Ambiguous Nucleotide Codes: double base codes
        | R -> W
        | Y -> S
        | K -> M
        | M -> K
        | S -> Y
        | W -> R
        // 'Ambiguous Nucleotide Codes: triple base codes
        | B -> V
        | D -> H
        | H -> D
        | V -> B
        
        | _    -> nuc

    /// Create the antiparallel DNA or RNA strand. For example, the sequence "ATGC" is converted to "GCAT". "Antiparallel" combines the two functions "Complement" and "Inverse".
    let antiparallel (nuc:Nucleotide) = 
        inverse (complement nuc)
            
        
    /// Replace thymidine (T) by uracil (U). For example, the sequence "ATUGC" is converted to "AUUGC".
    let replaceTbyU (nuc:Nucleotide) =          
        match nuc with
        | T -> U
        | _ -> nuc


    /// Replace uracil (U) by thymidine (T). For example, the sequence "ATUGC" is converted to "ATTGC".
    let replaceUbyT (nuc:Nucleotide) =
        match nuc with
        | U -> T
        | _ -> nuc



    /// Codon to AminoAcid 
    let CodonMap = [((U,U,U), AminoAcid.Phe);
                    ((U,U,C), AminoAcid.Phe);
                    ((U,U,A), AminoAcid.Leu);
                    ((U,U,G), AminoAcid.Leu);

                    ((U,C,U), AminoAcid.Ser);
                    ((U,C,C), AminoAcid.Ser);
                    ((U,C,A), AminoAcid.Ser);
                    ((U,C,G), AminoAcid.Ser);

                    ((U,A,U), AminoAcid.Tyr);
                    ((U,A,C), AminoAcid.Tyr);
                    ((U,A,A), AminoAcid.Ter);
                    ((U,A,G), AminoAcid.Ter);

                    ((U,G,U), AminoAcid.Cys);
                    ((U,G,C), AminoAcid.Cys);
                    ((U,G,A), AminoAcid.Ter);
                    ((U,G,G), AminoAcid.Trp);

                    ((C,U,U), AminoAcid.Leu);
                    ((C,U,C), AminoAcid.Leu);
                    ((C,U,A), AminoAcid.Leu);
                    ((C,U,G), AminoAcid.Leu);

                    ((C,C,U), AminoAcid.Pro);
                    ((C,C,C), AminoAcid.Pro);
                    ((C,C,A), AminoAcid.Pro);
                    ((C,C,G), AminoAcid.Pro);

                    ((C,A,U), AminoAcid.His);
                    ((C,A,C), AminoAcid.His);
                    ((C,A,A), AminoAcid.Gln);
                    ((C,A,G), AminoAcid.Gln);

                    ((C,G,U), AminoAcid.Arg);
                    ((C,G,C), AminoAcid.Arg);
                    ((C,G,A), AminoAcid.Arg);
                    ((C,G,G), AminoAcid.Arg);

                    ((A,U,U), AminoAcid.Ile);
                    ((A,U,C), AminoAcid.Ile);
                    ((A,U,A), AminoAcid.Ile);
                    ((A,U,G), AminoAcid.Met);

                    ((A,C,U), AminoAcid.Thr);
                    ((A,C,C), AminoAcid.Thr);
                    ((A,C,A), AminoAcid.Thr);
                    ((A,C,G), AminoAcid.Thr);

                    ((A,A,U), AminoAcid.Asn);
                    ((A,A,C), AminoAcid.Asn);
                    ((A,A,A), AminoAcid.Lys);
                    ((A,A,G), AminoAcid.Lys);

                    ((A,G,U), AminoAcid.Ser);
                    ((A,G,C), AminoAcid.Ser);
                    ((A,G,A), AminoAcid.Arg);
                    ((A,G,G), AminoAcid.Arg);

                    ((G,U,U), AminoAcid.Val);
                    ((G,U,C), AminoAcid.Val);
                    ((G,U,A), AminoAcid.Val);
                    ((G,U,G), AminoAcid.Val);

                    ((G,C,U), AminoAcid.Ala);
                    ((G,C,C), AminoAcid.Ala);
                    ((G,C,A), AminoAcid.Ala);
                    ((G,C,G), AminoAcid.Ala);

                    ((G,A,U), AminoAcid.Asp);
                    ((G,A,C), AminoAcid.Asp);
                    ((G,A,A), AminoAcid.Glu);
                    ((G,A,G), AminoAcid.Glu);

                    ((G,G,U), AminoAcid.Gly);
                    ((G,G,C), AminoAcid.Gly);
                    ((G,G,A), AminoAcid.Gly);
                    ((G,G,G), AminoAcid.Gly); ] |> Map.ofSeq
                    

    /// Codon to AminoAcid 
    let AmbiguousCodonMap = 
                   [((G,C,N), AminoAcid.Ala);
                    ((C,G,N), AminoAcid.Arg);
                    ((M,G,R), AminoAcid.Arg);

                    ((A,A,Y), AminoAcid.Asn);
                    ((G,A,Y), AminoAcid.Asp);
                    ((U,G,Y), AminoAcid.Cys);
                    ((C,A,R), AminoAcid.Cys);
                    ((G,A,R), AminoAcid.Glu);
                    ((G,G,N), AminoAcid.Gly);
                    ((C,A,Y), AminoAcid.His);
                    ((A,U,H), AminoAcid.Ile);
                    ((Y,U,R), AminoAcid.Leu);
                    ((C,U,N), AminoAcid.Leu);                    
                    ((A,A,R), AminoAcid.Lys);
                    ((U,U,Y), AminoAcid.Phe);
                    ((C,C,N), AminoAcid.Pro);
                    ((U,C,N), AminoAcid.Ser);
                    ((A,G,Y), AminoAcid.Ser);
                    ((A,C,N), AminoAcid.Thr);
                    ((U,A,Y), AminoAcid.Tyr);                    
                    ((G,U,N), AminoAcid.Val);

                    ((U,A,R), AminoAcid.Ter);
                    ((U,R,A), AminoAcid.Ter); ] |> Map.ofSeq


    /// <summary>
    /// Lookup an amino acid based on a triplet of nucleotides. U U U for instance
    /// will result in Phenylalanine.  If the values cannot be
    /// found in the lookup table, <c>false</c> will be returned.
    /// </summary>
    /// <param name="n1">The first character.</param>
    /// <param name="n2">The second character.</param>
    /// <param name="n3">The third character.</param>
    /// <returns>True/False if the value exists</returns>
    let lookupBytes (n1 : Nucleotide, n2 : Nucleotide, n3 : Nucleotide ) =
        
        // TODO: 
        let codon = (n1,n2,n3)
        CodonMap.Item(codon)




    /// Returns the name of nucleotide
    let name (nuc:Nucleotide) =
        BioItem.name nuc

    //Returns nucleotide formulas minus H20            
    let formula (nuc:Nucleotide) =
        BioItem.formula nuc
    
    /// Returns the symbol of AminoAcid       
    let symbol (nuc:Nucleotide) =
        BioItem.symbol nuc

    /// Returns true if nucleotide represents a sequence terminator
    let isTerminator (nuc:Nucleotide) =
        BioItem.isTerminator nuc

    /// Returns true if nucleotide represents a sequence gap
    let isGap (nuc:Nucleotide) =
        BioItem.isGap nuc

    /// Returns the monoisotopic mass of nucleotide (without H20)
    let monoisoMass (nuc:Nucleotide) =
        BioItem.monoisoMass nuc

    /// Returns the average mass of nucleotide (without H20)
    let averageMass (nuc:Nucleotide) =
        BioItem.averageMass nuc

    /// Returns a function to calculate the monoisotopic mass of a nucleotide with memoization
    let initMonoisoMassWithMemP =
        Memoization.memoizeP (fun a -> monoisoMass a)          

    /// Returns a function to calculate the average mass of a nucleotide with memoization
    let initAverageMassWithMemP = 
        Memoization.memoizeP (fun a -> averageMass a)
        




//    /// Properties of a nucleatide like formula, name, symbole, but also Physicochemical features
//    module Properties = 
//        
//        let a = 42
            




