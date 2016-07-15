namespace BioFSharp

type IBioSequence<[<EqualityConditionalOn; ComparisonConditionalOn >]'a when 'a :> IBioItem> =  seq<'a>

module IBioSequence =
    
    open FSharp.Care

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

        
        /// Converters char to AminoAcid option by ignoring bad character and ambiguis code
        let charToOptionStandardAminoAcid (aac:char) =
            let pac = AminoAcids.charToParsedAminoAcidChar aac
            match pac with
            | AminoAcids.ParsedAminoAcidChar.StandardCodes  (aa) -> Some aa
            | AminoAcids.ParsedAminoAcidChar.GapTer         (aa) -> Some aa 
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
            
                
    /// Generates amino acid sequence of one-letter-code string using given OptionConverter
    let ofAminoAcidStringWithOptionConverter (converter:OptionConverter.AminoAcidOptionConverter) (s:#seq<char>) : IBioSequence<_> =          
        s
        |> Seq.choose converter


    /// Generates amino acid sequence of one-letter-code raw string
    let ofAminoAcidString (s:#seq<char>) : IBioSequence<_> =          
        s
        |> Seq.choose OptionConverter.charToOptionAminoAcid


    /// Generates nucleotide sequence of one-letter-code string using given OptionConverter
    let ofNucleotideStringWithOptionConverter (converter:OptionConverter.NucleotideOptionConverter) (s:#seq<char>) : IBioSequence<_> =             
        s
        |> Seq.choose converter
        
    /// Generates nucleotide sequence of one-letter-code raw string
    let ofNucleotideString (s:#seq<char>) : IBioSequence<_> =             
        s
        |> Seq.choose OptionConverter.charToOptionNucleotid           


    
    ///Active pattern which returns a base triplet
    let private (|Triplet|_|) (en:System.Collections.Generic.IEnumerator<_>) = 
        if en.MoveNext () then                
                    let n1 = en.Current
                    if en.MoveNext () then
                        let n2 = en.Current
                        if en.MoveNext () then
                            Some((n1,n2,en.Current))
                        else
                            None
                    else
                        None
                    
        else
            None

    /// Builts a new collection whose elements are the result of applying
    /// the given function to each triplet of the collection. 
    let mapInTriplets f (input:seq<'a>) =
        let sourceIsEmpty = ref false    
        seq {   use en = input.GetEnumerator()
                while not(!sourceIsEmpty) do                
                match en with
                | Triplet t -> yield (f t)                                                              
                | _         -> sourceIsEmpty := true                               
        }

    //  Replace T by U
    /// Transcribe a given DNA coding strand (5'-----3')
    let transcribeCodeingStrand (nucs:seq<Nucleotides.Nucleotide>) = 
        nucs |> Seq.map (fun nuc -> Nucleotides.replaceTbyU nuc)
        


    //  
    /// Transcribe a given DNA template strand (3'-----5')
    let transcribeTemplateStrand (nucs:seq<Nucleotides.Nucleotide>) = 
        nucs |> Seq.map (fun nuc -> Nucleotides.replaceTbyU (Nucleotides.complement nuc))


    /// translates nucleotide sequence to aminoacid sequence    
    let translate (nucleotideOffset:int) (rnaSeq:seq<Nucleotides.Nucleotide>) =         
        if (nucleotideOffset < 0) then
                raise (System.ArgumentException(sprintf "Input error: nucleotide offset of %i is invalid" nucleotideOffset))                
        rnaSeq
        |> Seq.skip nucleotideOffset
        |> mapInTriplets Nucleotides.lookupBytes

    
    /// Compares the elemens of two biosequence
    let isEqual a b =
        let tmp = Seq.compareWith (fun elem1 elem2 ->
                            if elem1 = elem2 then 0    
                            else 1)  a b 
        tmp = 0



    /// Returns string of one-letter-code
    let toString (bs:seq<#IBioItem>) =
        new string [|for c in bs  -> BioItem.symbol c|]         


       
    /// Returns formula
    let toFormula (bs:seq<#IBioItem>) =
        bs |> Seq.fold (fun acc item -> Formula.add acc  (BioItem.formula item)) Formula.emptyFormula
        

    /// Returns monoisotopic mass of the given sequence !memoization
    let toMonoisotopicMass<'a when 'a :> IBioItem> : (seq<'a> -> float) =        
        let memMonoisoMass =
            Memoization.memoizeP (BioItem.formula >> Formula.monoisoMass)
        (fun bs -> 
            bs 
            |> Seq.sumBy memMonoisoMass)


    /// Returns average mass of the given sequence !memoization
    let toAverageMass<'a when 'a :> IBioItem> : (seq<'a> -> float) =
        let memAverageMass =
            Memoization.memoizeP (BioItem.formula >> Formula.averageMass)
        (fun bs -> 
            bs 
            |> Seq.sumBy memAverageMass)


    /// Returns monoisotopic mass of the given sequence including water (+H20)
    let toMonoisotopicMassWithWater<'a when 'a :> IBioItem> : (seq<'a> -> float) =
        let water = Formula.Table.H2O |> Formula.averageMass
        let memMonoisoMass =
            Memoization.memoizeP (BioItem.formula >> Formula.monoisoMass)
        (fun bs -> 
            bs 
            |> Seq.sumBy memMonoisoMass
            |> (+) water )


    /// Returns average mass of the given sequence including water (+H20)
    let toAverageMassWithWater<'a when 'a :> IBioItem> : (seq<'a> -> float) =
        let water = Formula.Table.H2O |> Formula.averageMass
        let memAverageMass =
            Memoization.memoizeP (BioItem.formula >> Formula.averageMass)
        (fun bs -> 
            bs 
            |> Seq.sumBy memAverageMass
            |> (+) water )


