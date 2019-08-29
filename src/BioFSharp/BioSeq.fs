namespace BioFSharp

///This module contains the BioSeq type and its according functions. The BioSeq type is a sequence of objects using the IBioItem interface
module BioSeq =

    open FSharpAux
    open BioFSharp.BioItemsConverter

    ///Sequence of objects using the IBioItem interface
    type BioSeq<[<EqualityConditionalOn; ComparisonConditionalOn >]'a when 'a :> IBioItem> = seq<'a>

    /// Generates AminoAcid sequence of one-letter-code string using given OptionConverter
    let ofAminoAcidStringWithOptionConverter (converter:OptionConverter.AminoAcidOptionConverter) (s:#seq<char>) : BioSeq<_> =          
        s
        |> Seq.choose converter


    /// Generates AminoAcid sequence of one-letter-code raw string
    let ofAminoAcidString (s:#seq<char>) : BioSeq<_> =          
        s
        |> Seq.choose OptionConverter.charToOptionAminoAcid

    /// Generates AminoAcidSymbol sequence of one-letter-code raw string
    let ofAminoAcidSymbolString (s:#seq<char>) : BioSeq<_> =          
        s
        |> Seq.choose (AminoAcidSymbols.parseChar >> snd)

    /// Generates nucleotide sequence of one-letter-code string using given OptionConverter
    let ofNucleotideStringWithOptionConverter (converter:OptionConverter.NucleotideOptionConverter) (s:#seq<char>) : BioSeq<_> =             
        s
        |> Seq.choose converter
        
    /// Generates nucleotide sequence of one-letter-code raw string
    let ofNucleotideString (s:#seq<char>) : BioSeq<_> =             
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


    /// Create the reverse DNA or RNA strand. For example, the sequence "ATGC" is converted to "CGTA"
    let reverse (nucs:seq<Nucleotides.Nucleotide>) : BioSeq<_> = 
        nucs |> Seq.rev

    /// Create the complement DNA or cDNA (from RNA) strand. For example, the sequence "ATGC" is converted to "TACG"
    let complement (nucs:seq<Nucleotides.Nucleotide>) : BioSeq<_> = 
        nucs |> Seq.map Nucleotides.complement

    /// Create the reverse complement strand meaning antiparallel DNA strand or the cDNA (from RNA) respectivly. For example, the sequence "ATGC" is converted to "GCAT". "Antiparallel" combines the two functions "Complement" and "Inverse".
    let reverseComplement (nucs:seq<Nucleotides.Nucleotide>) : BioSeq<_> = 
        nucs |> Seq.map Nucleotides.complement |> Seq.rev

    //  Replace T by U
    /// Transcribe a given DNA coding strand (5'-----3')
    let transcribeCodingStrand (nucs:seq<Nucleotides.Nucleotide>) : BioSeq<_> = 
        nucs |> Seq.map (fun nuc -> Nucleotides.replaceTbyU nuc)
        
    //  
    /// Transcribe a given DNA template strand (3'-----5')
    let transcribeTemplateStrand (nucs:seq<Nucleotides.Nucleotide>) : BioSeq<_> = 
        nucs |> Seq.map (fun nuc -> Nucleotides.replaceTbyU (Nucleotides.complement nuc))


    /// translates nucleotide sequence to aminoacid sequence    
    let translate (nucleotideOffset:int) (rnaSeq:seq<Nucleotides.Nucleotide>) : BioSeq<_> =         
        if (nucleotideOffset < 0) then
                raise (System.ArgumentException(sprintf "Input error: nucleotide offset of %i is invalid" nucleotideOffset))                
        rnaSeq
        |> Seq.skip nucleotideOffset
        |> mapInTriplets Nucleotides.lookupBytes

    
    /// Compares the elemens of two sequence
    let isEqual a b =
        Seq.compareWith 
            (fun elem1 elem2 ->
                if elem1 = elem2 then 0    
                else 1)  a b 
        



    /// Returns string of one-letter-code
    let toString (bs:seq<#IBioItem>) =
        new string [|for c in bs  -> BioItem.symbol c|]         


       
    /// Returns formula
    let toFormula (bs:seq<#IBioItem>) =
        bs |> Seq.fold (fun acc item -> Formula.add acc  (BioItem.formula item)) Formula.emptyFormula
        

    /// Returns monoisotopic mass of the given sequence
    let toMonoisotopicMass (bs:seq<#IBioItem>) =
        bs |> Seq.sumBy BioItem.monoisoMass


    /// Returns average mass of the given sequence
    let toAverageMass (bs:seq<#IBioItem>) =
        bs |> Seq.sumBy BioItem.averageMass


    /// Returns monoisotopic mass of the given sequence and initial value (e.g. H2O) 
    let toMonoisotopicMassWith (state) (bs:seq<#IBioItem>) =
        bs |> Seq.fold (fun massAcc item -> massAcc + BioItem.monoisoMass item) state


    /// Returns average mass of the given sequence and initial value (e.g. H2O) 
    let toAverageMassWith (state) (bs:seq<#IBioItem>) =
        bs |> Seq.fold (fun massAcc item -> massAcc + BioItem.averageMass item) state


    /// Returns a function to calculate the monoisotopic mass of the given sequence !memoization
    let initMonoisoMass<'a when 'a :> IBioItem> : (seq<'a> -> float) =        
        let memMonoisoMass =
            Memoization.memoizeP (BioItem.formula >> Formula.monoisoMass)
        (fun bs -> 
            bs 
            |> Seq.sumBy memMonoisoMass)


    /// Returns a function to calculate the average mass of the given sequence !memoization
    let initAverageMass<'a when 'a :> IBioItem> : (seq<'a> -> float) =
        let memAverageMass =
            Memoization.memoizeP (BioItem.formula >> Formula.averageMass)
        (fun bs -> 
            bs 
            |> Seq.sumBy memAverageMass)


    /// Returns a function to calculate the monoisotopic mass of the given sequence and initial value (e.g. H2O) !memoization
    let initMonoisoMassWith<'a when 'a :> IBioItem> (state:float) : (seq<'a> -> float)  =        
        let memMonoisoMass =
            Memoization.memoizeP (BioItem.formula >> Formula.monoisoMass)
        (fun bs -> 
            bs |> Seq.fold (fun massAcc item -> massAcc + memMonoisoMass item) state)


    /// Returns a function to calculate the average mass of the given sequence and initial value (e.g. H2O) !memoization
    let initAverageMassWith<'a when 'a :> IBioItem> (state:float) : (seq<'a> -> float) =
        let memAverageMass =
            Memoization.memoizeP (BioItem.formula >> Formula.averageMass)
        (fun bs -> 
            bs |> Seq.fold (fun massAcc item -> massAcc + memAverageMass item) state)

    ///Creates an array with information about the abundacies of the distinct BioItems by converting the symbol of the BioItem to an integer and incrementing the given integer. To decrease the size of the resulting array by still having a fast performance, all indices are shifted by 65. Therefore to call the abundancy of a given BioItem, use "Resultcompositionvector.[(BioItem.symbol bioitem) - 65]"
    let toCompositionVector (input:BioSeq<_>)  =
        let compVec = Array.zeroCreate 26
        input
        |> Seq.iter (fun a ->                         
                            let index = (int (BioItem.symbol a)) - 65
                            compVec.[index] <- compVec.[index] + 1)
        compVec   







//    /// Returns monoisotopic mass of the given sequence including water (+H20)
//    let toMonoisotopicMassWithWater<'a when 'a :> IBioItem> : (seq<'a> -> float) =
//        let water = Formula.Table.H2O |> Formula.averageMass
//        let monoMass' = (BioItem.formula >> Formula.monoisoMass)
//        let memMonoisoMass =
//            Memoization.memoizeP monoMass'
//        (fun bs -> 
//            bs 
//            |> Seq.sumBy memMonoisoMass
//            |> (+) water )
//
//
//    /// Returns average mass of the given sequence including water (+H20)
//    let toAverageMassWithWater<'a when 'a :> IBioItem> : (seq<'a> -> float) =
//        let water = Formula.Table.H2O |> Formula.averageMass        
//        let averageMass' = (BioItem.formula >> Formula.averageMass)
//        let memAverageMass =
//            Memoization.memoizeP averageMass'
//        (fun bs -> 
//            bs 
//            |> Seq.sumBy memAverageMass
//            |> (+) water )

