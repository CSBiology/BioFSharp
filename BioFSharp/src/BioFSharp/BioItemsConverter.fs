namespace BioFSharp

///Contains Functionalities for parsing Bioitems
module BioItemsConverter =
    
    
    open FSharp.Care
    ///Contains Functionalities for trying to parse Bioitems
    module OptionConverter =  
        
        /// Type abbreviation for converting char to optional Nucleotide
        type NucleotideOptionConverter = char -> Nucleotides.Nucleotide option
        /// Type abbreviation for converting char to optional AminoAcid
        type AminoAcidOptionConverter = char -> AminoAcids.AminoAcid option

        /// Converters char to AminoAcid option by ignoring bad character
        let charToOptionAminoAcid (aac:char) =
            let pac = AminoAcids.charToParsedAminoAcidChar aac
            match pac with
            | AminoAcids.ParsedAminoAcidChar.StandardCodes  (aa) -> Some aa
            | AminoAcids.ParsedAminoAcidChar.AmbiguityCodes (aa) -> Some aa 
            | AminoAcids.ParsedAminoAcidChar.GapTer         (aa) -> Some aa 
            | AminoAcids.ParsedAminoAcidChar.NoAAChar (_)        -> None                

        /// Converters char to AminoAcid option by ignoring bad character
        /// Ignores Gap and Ter
        let charToOptionAminoAcidWithoutGapTer (aac:char) =
            let pac = AminoAcids.charToParsedAminoAcidChar aac
            match pac with
            | AminoAcids.ParsedAminoAcidChar.StandardCodes  (aa) -> Some aa
            | AminoAcids.ParsedAminoAcidChar.AmbiguityCodes (aa) -> Some aa 
            | AminoAcids.ParsedAminoAcidChar.GapTer         (_)  -> None
            | AminoAcids.ParsedAminoAcidChar.NoAAChar (_)        -> None
        
        /// Converters char to AminoAcid option by ignoring bad character and ambiguis code        
        let charToOptionStandardAminoAcid (aac:char) =
            let pac = AminoAcids.charToParsedAminoAcidChar aac
            match pac with
            | AminoAcids.ParsedAminoAcidChar.StandardCodes  (aa) -> Some aa
            | AminoAcids.ParsedAminoAcidChar.GapTer         (aa) -> Some aa 
            | AminoAcids.ParsedAminoAcidChar.AmbiguityCodes (_)  -> None
            | AminoAcids.ParsedAminoAcidChar.NoAAChar (_)        -> None

        /// Converters char to AminoAcid option by ignoring bad character and ambiguis code
        /// Ignores Gap and Ter
        let charToOptionStandardAminoAcidWithoutGapTer (aac:char) =
            let pac = AminoAcids.charToParsedAminoAcidChar aac
            match pac with
            | AminoAcids.ParsedAminoAcidChar.StandardCodes  (aa) -> Some aa
            | AminoAcids.ParsedAminoAcidChar.GapTer         (_)  -> None 
            | AminoAcids.ParsedAminoAcidChar.AmbiguityCodes (_)  -> None
            | AminoAcids.ParsedAminoAcidChar.NoAAChar (_)        -> None

        /// Converters char to AminoAcid option by ignoring bad character
        let charToOptionNucleotid (nuc:char) =
            let pnc = Nucleotides.charToParsedNucleotideChar nuc
            match pnc with
            | Nucleotides.ParsedNucleotideChar.StandardCodes    (n) -> Some n
            | Nucleotides.ParsedNucleotideChar.Standard_DNAonly (n) -> Some n
            | Nucleotides.ParsedNucleotideChar.Standard_RNAonly (n) -> Some n
            | Nucleotides.ParsedNucleotideChar.AmbiguityCodes   (n) -> Some n
            | Nucleotides.ParsedNucleotideChar.GapTer           (n) -> Some n 
            | Nucleotides.ParsedNucleotideChar.NoNucChar (_)        -> None              


        /// Converters char to AminoAcid option by ignoring bad character
        let charToOptionStandardNucleotid (nuc:char) =
            let pnc = Nucleotides.charToParsedNucleotideChar nuc
            match pnc with
            | Nucleotides.ParsedNucleotideChar.StandardCodes    (n) -> Some n
            | Nucleotides.ParsedNucleotideChar.Standard_DNAonly (n) -> Some n
            | Nucleotides.ParsedNucleotideChar.Standard_RNAonly (n) -> Some n
            | Nucleotides.ParsedNucleotideChar.AmbiguityCodes   (_) -> None
            | Nucleotides.ParsedNucleotideChar.GapTer           (n) -> Some n
            | Nucleotides.ParsedNucleotideChar.NoNucChar (_)        -> None  
