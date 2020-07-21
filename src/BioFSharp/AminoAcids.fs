namespace BioFSharp

open FSharpAux
open ModificationInfo
open System

//Remarks via https://en.wikipedia.org/wiki/Proteinogenic_amino_acid

///Contains the AminoAcid type and its according functions. The AminoAcid type is a complex presentation of amino acids, allowing modifications
module AminoAcids =

    /// Amino acid Codes
    [<StructuralEquality;StructuralComparison>]
    type AminoAcid =
        /// 'A' - Ala - Alanine
        ///
        /// Very abundant and very versatile, it is more stiff than glycine, but small enough to pose only small steric limits for the protein conformation. It behaves fairly neutrally, and can be located in both hydrophilic regions on the protein outside and the hydrophobic areas inside.
        | Ala        
        /// 'C' - Cys - Cysteine
        ///
        /// The sulfur atom bonds readily to heavy metal ions. Under oxidizing conditions, two cysteines can join together in a disulfide bond to form the amino acid cystine. When cystines are part of a protein, insulin for example, the tertiary structure is stabilized, which makes the protein more resistant to denaturation; therefore, disulfide bonds are common in proteins that have to function in harsh environments including digestive enzymes (e.g., pepsin and chymotrypsin) and structural proteins (e.g., keratin). Disulfides are also found in peptides too small to hold a stable shape on their own (e.g. insulin).
        | Cys
        /// 'D' - Asp - Aspartic Acid
        ///
        /// Asp behaves similarly to glutamic acid, and carries a hydrophilic acidic group with strong negative charge. Usually, it is located on the outer surface of the protein, making it water-soluble. It binds to positively charged molecules and ions, and is often used in enzymes to fix the metal ion. When located inside of the protein, aspartate and glutamate are usually paired with arginine and lysine.
        | Asp
        /// 'E' - Glu - Glutamic Acid
        ///
        /// Glu behaves similarly to aspartic acid, and has a longer, slightly more flexible side chain.
        | Glu 
        /// 'F' - Phe - Phenylalanine
        ///
        /// Essential for humans, phenylalanine, tyrosine, and tryptophan contain a large, rigid aromatic group on the side chain. These are the biggest amino acids. Like isoleucine, leucine, and valine, these are hydrophobic and tend to orient towards the interior of the folded protein molecule. Phenylalanine can be converted into tyrosine.
        | Phe
        /// 'G' - Gly - Glycine
        ///
        /// Because of the two hydrogen atoms at the α carbon, glycine is not optically active. It is the smallest amino acid, rotates easily, and adds flexibility to the protein chain. It is able to fit into the tightest spaces, e.g., the triple helix of collagen. As too much flexibility is usually not desired, as a structural component, it is less common than alanine.
        | Gly
        /// 'H' - His - Histidine
        ///
        /// His is essential for humans. In even slightly acidic conditions, protonation of the nitrogen occurs, changing the properties of histidine and the polypeptide as a whole. It is used by many proteins as a regulatory mechanism, changing the conformation and behavior of the polypeptide in acidic regions such as the late endosome or lysosome, enforcing conformation change in enzymes. However, only a few histidines are needed for this, so it is comparatively scarce.
        | His
        /// 'I' - Ile - Isoleucine
        ///
        /// Ile is essential for humans. Isoleucine, leucine, and valine have large aliphatic hydrophobic side chains. Their molecules are rigid, and their mutual hydrophobic interactions are important for the correct folding of proteins, as these chains tend to be located inside of the protein molecule.
        | Ile
        /// 'K' - Lys - Lysine
        ///
        /// Lys is essential for humans, and behaves similarly to arginine. It contains a long, flexible side chain with a positively charged end. The flexibility of the chain makes lysine and arginine suitable for binding to molecules with many negative charges on their surfaces. E.g., DNA-binding proteins have their active regions rich with arginine and lysine. The strong charge makes these two amino acids prone to be located on the outer hydrophilic surfaces of the proteins; when they are found inside, they are usually paired with a corresponding negatively charged amino acid, e.g., aspartate or glutamate.
        | Lys
        /// 'L' - Leu - Leucine
        ///
        /// Leu is essential for humans, and behaves similarly to isoleucine and valine.
        | Leu
        /// 'M' - Met - Methionine
        ///
        /// Met is essential for humans. Always the first amino acid to be incorporated into a protein, it is sometimes removed after translation. Like cysteine, it contains sulfur, but with a methyl group instead of hydrogen. This methyl group can be activated, and is used in many reactions where a new carbon atom is being added to another molecule.
        | Met
        /// 'N' - Asn - Asparagine
        ///
        /// Similar to aspartic acid, Asn contains an amide group where Asp has a carboxyl.
        | Asn
        /// 'O' - Pyl - Pyrrolysine
        ///
        /// Similar to lysine, but it has a pyrroline ring attached. In some methanogenic prokaryotes, the UAG codon (normally a stop codon) can also be translated to pyrrolysine.
        | Pyl
        /// 'P' - Pro - Proline
        ///
        /// Pro contains an unusual ring to the N-end amine group, which forces the CO-NH amide sequence into a fixed conformation. It can disrupt protein folding structures like α helix or β sheet, forcing the desired kink in the protein chain. Common in collagen, it often undergoes a post-translational modification to hydroxyproline.
        | Pro        
        /// 'Q' - Gln - Glutamine
        ///
        /// Similar to glutamic acid, Gln contains an amide group where Glu has a carboxyl. Used in proteins and as a storage for ammonia, it is the most abundant amino acid in the body.
        | Gln
        /// 'R' - Arg - Arginine
        ///
        /// Functionally similar to lysine.
        | Arg
        /// 'S' - Ser - Serine
        ///
        /// Serine and threonine have a short group ended with a hydroxyl group. Its hydrogen is easy to remove, so serine and threonine often act as hydrogen donors in enzymes. Both are very hydrophilic, so the outer regions of soluble proteins tend to be rich with them.
        | Ser        
        /// 'T' - Thr - Threonine
        ///
        /// Essential for humans, Thr behaves similarly to serine.
        | Thr
        /// 'U' - Sel - Selenocysteine
        ///
        /// The selenium analog of cysteine, in which selenium replaces the sulfur atom.
        /// Warning: 'Sel' is not the official UPAC abbreviation. 
        /// This case will be removed in favor of 'Sec' in the next major release
        | [<Obsolete("This case has a typo and will be removed in the next major release. use AminoAcid.Sec instead.")>] Sel 
        /// 'U' - Sec - Selenocysteine
        ///
        /// The selenium analog of cysteine, in which selenium replaces the sulfur atom.
        | Sec
        /// 'V' - Val - Valine
        ///
        /// Essential for humans, Val behaves similarly to isoleucine and leucine.
        | Val
        /// 'W' - Trp - Tryptophan
        ///
        /// Essential for humans, Trp behaves similarly to phenylalanine and tyrosine. It is a precursor of serotonin and is naturally fluorescent.
        | Trp
        /// 'Y' - Tyr - Tyrosine
        ///
        /// Tyr behaves similarly to phenylalanine (precursor to tyrosine) and tryptophan, and is a precursor of melanin, epinephrine, and thyroid hormones. Naturally fluorescent, its fluorescence is usually quenched by energy transfer to tryptophans.
        | Tyr

        /// 'X' - Xaa - Unspecified 
        ///
        /// Placeholder in a sequence for any amino acid
        | Xaa        
        /// 'J'  *Leucine/Isoleucine
        ///
        /// Placeholder in a sequence for either Leucine or Isoleucine
        | Xle
        /// 'Z' - Glx - Glutamine/glutamic acid
        ///
        /// Placeholder in a sequence for either Glutamine or Glutamic Acid
        | Glx
        /// 'B' - Asx - Asparagine/aspartic acid
        ///
        /// Placeholder in a sequence for either Asparagine or Aspartic Acid
        | Asx

        /// '-'  Gap 
        | Gap
        /// '*'  Termination
        | Ter
        /// Modified AminoAcid
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
                            | AminoAcid.Sel | AminoAcid.Sec -> 'U'
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
                            | AminoAcid.Sel | AminoAcid.Sec -> Formula.Table.Sel // Selenocysteine
                            | AminoAcid.Val -> Formula.Table.Val 
                            | AminoAcid.Trp -> Formula.Table.Trp 
                            | AminoAcid.Tyr -> Formula.Table.Tyr 
                                                                
                            | AminoAcid.Xaa -> Formula.Table.Xaa  // Averagine Model -> C4.9384 H7.7583 N1.3577 O1.4773 S0.0417
                            | AminoAcid.Xle -> Formula.Table.Xle 
                            | AminoAcid.Glx -> Formula.Table.Glx 
                            | AminoAcid.Asx -> Formula.Table.Asx 
           
                            | AminoAcid.Gap -> (Formula.emptyFormula)
                            | AminoAcid.Ter -> (Formula.emptyFormula)

                            | AminoAcid.Mod (aa,mds) -> 
                                let isoMods     = List.filter (fun (md:Modification) -> md.Location = ModLocation.Isotopic) mds     
                                let bioMods     = List.filter (fun (md:Modification) -> md.Location <> ModificationInfo.ModLocation.Isotopic && md.IsBiological) mds     
                                let nonBioMods  = List.filter (fun (md:Modification) -> md.Location <> ModificationInfo.ModLocation.Isotopic && not md.IsBiological) mds     
                                List.fold (fun acc (md:ModificationInfo.Modification) -> md.Modify acc ) (formula aa) bioMods
                                |> fun f -> List.fold (fun acc (md:ModificationInfo.Modification) -> md.Modify acc ) f isoMods
                                |> fun f -> List.fold (fun acc (md:ModificationInfo.Modification) -> md.Modify acc ) f nonBioMods
                                //List.fold (fun acc (md:ModificationInfo.Modification) -> md.Modify acc ) (formula aa) mds
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
                            | AminoAcid.Ala -> "Alanine"          
                            | AminoAcid.Cys -> "Cysteine"       
                            | AminoAcid.Asp -> "Aspartic Acid"  
                            | AminoAcid.Glu -> "Glutamic Acid"  
                            | AminoAcid.Phe -> "Phenylalanine"   
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
                            | AminoAcid.Sel | AminoAcid.Sec -> "Selenocysteine" 
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


    /// Sets amino acid modification 
    let setModification (md:ModificationInfo.Modification) (aa:AminoAcid) =
        match aa with
        | Mod (a,mds) -> Mod ( a ,  md::mds )
        | _           -> Mod ( aa, [md] )

    /// Sets multiple amino acid modifications 
    let setModifications (md:ModificationInfo.Modification list) (aa:AminoAcid) =
        match aa with
        | Mod (a,mds) -> Mod ( a ,  md@mds )
        | _           -> Mod ( aa, md )

    /// Gets amino acid modifications 
    let getModifications (aa:AminoAcid) =
        match aa with
        | Mod (_,mds) -> mds
        | _           -> []

    /// Gets amino acid without the modifications 
    let getAminoAcidWithoutMod (aa:AminoAcid) =
        match aa with
        | Mod (aa',_)  -> aa'
        | _            -> aa


    /// Gets amino acid modifications 
    let tryGetModifications (aa:AminoAcid) =
        match aa with
        | Mod (_,mds) -> Some mds
        | _           -> None


    ///Modifies a formula f by applying all isotopic modifications of a given amino acid aa
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
        //| 'U' ->  StandardCodes AminoAcid.Sec 
        // ambiguous amino acids
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
    [<Obsolete("use aminoAcidSetStandard instead, or aminoAcidSetProteinogenic for all 22 AAs")>]
    let AminoAcidSetStandard = set [
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
        AminoAcid.Asn
        AminoAcid.Pyl
        AminoAcid.Pro
        AminoAcid.Gln
        AminoAcid.Arg
        AminoAcid.Ser
        AminoAcid.Thr
        AminoAcid.Sel
        AminoAcid.Sec
        AminoAcid.Val
        AminoAcid.Trp
        AminoAcid.Tyr 
    ]  

    ///Set of the 20 standard amino acids of the genetic code
    let aminoAcidSetStandard = set [
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
        AminoAcid.Asn
        AminoAcid.Pro
        AminoAcid.Gln
        AminoAcid.Arg
        AminoAcid.Ser
        AminoAcid.Thr
        AminoAcid.Val
        AminoAcid.Trp
        AminoAcid.Tyr 
    ]

    ///Set of all 22 proteinogenic amino acids (20 standard + Selenocysteine + Pyrrolysine)
    let aminoAcidSetProteinogenic = set [
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
        AminoAcid.Asn
        AminoAcid.Pro
        AminoAcid.Gln
        AminoAcid.Arg
        AminoAcid.Ser
        AminoAcid.Thr
        AminoAcid.Val
        AminoAcid.Trp
        AminoAcid.Tyr 
        AminoAcid.Sel 
        AminoAcid.Sec
        AminoAcid.Pyl
    ]

    ///Set of all 21 proteinogenic amino acids in eucaryotes (20 standard + Selenocysteine)
    let aminoAcidSetProteinogenicEucaryotes = set [
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
        AminoAcid.Asn
        AminoAcid.Pro
        AminoAcid.Gln
        AminoAcid.Arg
        AminoAcid.Ser
        AminoAcid.Thr
        AminoAcid.Val
        AminoAcid.Trp
        AminoAcid.Tyr 
        AminoAcid.Sel 
        AminoAcid.Sec
    ]
    
    [<Obsolete("use aminoAcidSetAmbiguity instead")>]
    ///Set of all ambiguous codes
    let AminoAcidSetAmbiguity = set [
        AminoAcid.Xaa
        AminoAcid.Xle
        AminoAcid.Glx
        AminoAcid.Asx 
    ]      
    
    ///Set of all ambiguous codes
    let aminoAcidSetAmbiguity = set [
        AminoAcid.Xaa
        AminoAcid.Xle
        AminoAcid.Glx
        AminoAcid.Asx 
    ]  
    
    [<Obsolete("use aminoAcidSetGapTer instead")>]
    ///Set containing the Gap and the Terminator AminoAcid
    let AminoAcidSetGapTer = set [
        AminoAcid.Gap
        AminoAcid.Ter 
    ]    
    
    ///Set containing the Gap and the Terminator AminoAcid
    let aminoAcidSetGapTer = set [
        AminoAcid.Gap
        AminoAcid.Ter 
    ]

    [<Obsolete("use aminoAcidSetPosCharged instead")>]
    ///Set of all AminoAcids with basic (positively charged) sidechain
    let AminoAcidSetPosCharged = set [
        AminoAcid.Arg
        AminoAcid.Lys
        AminoAcid.His 
    ]    
    
    ///Set of all AminoAcids with basic (positively charged) sidechain
    let aminoAcidSetPosCharged = set [
        AminoAcid.Arg
        AminoAcid.Lys
        AminoAcid.His 
    ]

    [<Obsolete("use aminoAcidSetNegCharged instead. Warning: this set also contains Cysteine and Tyrosine, which are not generally considered as neg charged under phisiological (pH 7.4) conditions. aminoAcidSetNegCharged will not contain them.")>]
    ///Set of all AminoAcids with acidic sidechain (+ Cys/Tyr)
    let AminoAcidSetNegCharged = set [
        AminoAcid.Asp
        AminoAcid.Glu
        AminoAcid.Cys
        AminoAcid.Tyr 
    ]    
    
    ///Set of all AminoAcids with acidic sidechain
    let aminoAcidSetNegCharged = set [
            AminoAcid.Asp
            AminoAcid.Glu
    ]

    [<Obsolete("use aminoAcidSetPolarUncharged instead. Warning: this set also contains Trp, His, Tyr, and Cys, which aminoAcidSetPolarUncharged will not")>]
    ///Set of all AminoAcids with polar sidechain
    let AminoAcidSetPolar = set [
        AminoAcid.Gln
        AminoAcid.Asn
        AminoAcid.His
        AminoAcid.Ser
        AminoAcid.Thr
        AminoAcid.Tyr
        AminoAcid.Cys
        AminoAcid.Trp 
    ]    
                
    ///Set of all AminoAcids with uncharged polar sidechain
    let aminoAcidSetPolarUncharged = set [
        AminoAcid.Gln
        AminoAcid.Asn
        AminoAcid.Ser
        AminoAcid.Thr
    ]

    [<Obsolete("use aminoAcidSetHydrophobic instead. Warning: aminoAcidSetHydrophobic will not contain Pro/Gly, but additionally Trp/Tyr")>]
    ///Set of all AminoAcids with hydrophobic sidechain
    let AminoAcidSetHydrophobic = set [
        AminoAcid.Ala
        AminoAcid.Ile
        AminoAcid.Leu
        AminoAcid.Met
        AminoAcid.Phe
        AminoAcid.Val
        AminoAcid.Gly 
        AminoAcid.Pro
    ]    
    
    ///Set of all AminoAcids with hydrophobic sidechain
    let aminoAcidSetHydrophobic = set [
        AminoAcid.Ala
        AminoAcid.Ile
        AminoAcid.Leu
        AminoAcid.Met
        AminoAcid.Phe
        AminoAcid.Trp
        AminoAcid.Tyr
        AminoAcid.Val
    ]

    let aminoAcidSetSpecialCases = set [
        AminoAcid.Cys
        AminoAcid.Sel
        AminoAcid.Sec
        AminoAcid.Gly
        AminoAcid.Pro
    ]
 
    /// Returns the name of AminoAcid
    let name (aa:AminoAcid) =
        BioItem.name aa

    ///Returns amino acid formulas minus H20            
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

    [<Obsolete("use isPolarUncharged instead")>]
    /// Returns true, if the AminoAcid has a polar side chain
    let isPolar (aa:AminoAcid) = 
        AminoAcidSetPolar.Contains aa    
        
    /// Returns true, if the AminoAcid has a polar, uncharged side chain
    let isPolarUncharged (aa:AminoAcid) = 
        aminoAcidSetPolarUncharged.Contains aa

    /// Returns true, if the AminoAcid has a hydrophobic side chain
    let isHydrophobic (aa:AminoAcid) = 
        AminoAcidSetHydrophobic.Contains aa    

    /// Returns true if AminoAcid contains a modification
    let isModified (aa:AminoAcid) =
        match aa with
        | Mod _ -> true
        | _ -> false        