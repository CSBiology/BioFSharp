namespace BioFSharp

///This module contains the BioArray type and its according functions. The BioArray type is an array of objects using the IBioItem interface
module BioArray =
    
    open System
    open FSharpAux
    open BioItemsConverter

    ///Array of objects using the IBioItem interface
    type BioArray<[<EqualityConditionalOn; ComparisonConditionalOn >]'a when 'a :> IBioItem> = array<'a>

    /// Generates amino acid sequence of one-letter-code string using given OptionConverter
    let ofAminoAcidStringWithOptionConverter (converter:OptionConverter.AminoAcidOptionConverter) (s:#seq<char>) : BioArray<_> =          
        s
        |> Seq.choose converter
        |> Seq.toArray

    /// Generates amino acid sequence of one-letter-code raw string
    let ofAminoAcidString (s:#seq<char>) : BioArray<_> =          
        s
        |> Seq.choose OptionConverter.charToOptionAminoAcid
        |> Seq.toArray
    
    /// Generates amino acid symbol sequence of one-letter-code raw string
    let ofAminoAcidSymbolString (s:#seq<char>) : BioArray<_> =          
        s
        |> Seq.choose (AminoAcidSymbols.parseChar >> snd)
        |> Seq.toArray

    /// Generates nucleotide sequence of one-letter-code string using given OptionConverter
    let ofNucleotideStringWithOptionConverter (converter:OptionConverter.NucleotideOptionConverter) (s:#seq<char>) : BioArray<_> =             
        s
        |> Seq.choose converter
        |> Seq.toArray

    /// Generates nucleotide sequence of one-letter-code raw string
    let ofNucleotideString (s:#seq<char>) : BioArray<_> =             
        s
        |> Seq.choose OptionConverter.charToOptionNucleotid    
        |> Seq.toArray

    /// Create the reverse DNA or RNA strand. For example, the sequence "ATGC" is converted to "CGTA"
    let reverse (nucs:BioArray<Nucleotides.Nucleotide>) : BioArray<_> = 
        nucs |> Array.rev

    /// Create the complement DNA or cDNA (from RNA) strand. For example, the sequence "ATGC" is converted to "TACG"
    let complement (nucs:BioArray<Nucleotides.Nucleotide>) : BioArray<_>= 
        nucs |> Array.map Nucleotides.complement

    /// Create the reverse complement strand meaning antiparallel DNA strand or the cDNA (from RNA) respectivly. For example, the sequence "ATGC" is converted to "GCAT". "Antiparallel" combines the two functions "Complement" and "Inverse".
    let reverseComplement (nucs:BioArray<Nucleotides.Nucleotide>) : BioArray<_>= 
        nucs |> Array.map Nucleotides.complement |> Array.rev

    /// Builts a new collection whose elements are the result of applying
    /// the given function to each triplet of the collection. 
    let mapInTriplets mapping (input:BioArray<'a>) =        
        Array.init (input.Length / 3) (fun i -> mapping (input.[i * 3],input.[(i*3)+1],input.[(i*3)+2]) )

    //  Replace T by U
    /// Transcribe a given DNA coding strand (5'-----3')
    [<Obsolete("This function name contained a typo and will be removed in the next major release. Use transcribeCodingStrand instead.")>]
    let transcribeCodeingStrand (nucs:BioArray<Nucleotides.Nucleotide>) : BioArray<_> = 
        nucs |> Array.map (fun nuc -> Nucleotides.replaceTbyU nuc)
        
    /// Transcribe a given DNA coding strand (5'-----3')
    let transcribeCodingStrand (nucs:BioArray<Nucleotides.Nucleotide>) : BioArray<_> = 
        nucs |> Array.map (fun nuc -> Nucleotides.replaceTbyU nuc)
        
    /// Transcribe a given DNA template strand (3'-----5')
    let transcribeTemplateStrand (nucs:BioArray<Nucleotides.Nucleotide>) : BioArray<_> =
        nucs |> Array.map (fun nuc -> Nucleotides.replaceTbyU (Nucleotides.complement nuc))

    /// translates nucleotide sequence to aminoacid sequence    
    let translate (nucleotideOffset:int) (rnaSeq:BioArray<Nucleotides.Nucleotide>) : BioArray<_> =         
        if (nucleotideOffset < 0) then
                raise (System.ArgumentException(sprintf "Input error: nucleotide offset of %i is invalid" nucleotideOffset))                
        rnaSeq
        |> Array.skip nucleotideOffset
        |> mapInTriplets Nucleotides.lookupBytes

    /// Compares the elemens of two biosequence
    let isEqual a b =
        Array.compareWith
            (fun elem1 elem2 ->
                if elem1 = elem2 then 0    
                else 1)  a b 
        
    /// Returns string of one-letter-code
    let toString (bs:BioArray<#IBioItem>) =
        new string (bs |> Array.map BioItem.symbol) 

   /// Returns monoisotopic mass of the given sequence
    let toMonoisotopicMass (bs:BioArray<#IBioItem>) =
        bs |> Array.sumBy BioItem.monoisoMass

    /// Returns average mass of the given sequence
    let toAverageMass (bs:BioArray<#IBioItem>) =
        bs |> Array.sumBy BioItem.averageMass

    /// Returns monoisotopic mass of the given sequence and initial value (e.g. H2O) 
    let toMonoisotopicMassWith (state) (bs:BioArray<#IBioItem>) =
        bs |> Array.fold (fun massAcc item -> massAcc + BioItem.monoisoMass item) state

    /// Returns average mass of the given sequence and initial value (e.g. H2O) 
    let toAverageMassWith (state) (bs:BioArray<#IBioItem>) =
        bs |> Array.fold (fun massAcc item -> massAcc + BioItem.averageMass item) state

    /// Returns a function to calculate the monoisotopic mass of the given sequence !memoization
    let initMonoisoMass<'a when 'a :> IBioItem> : (BioArray<_> -> float) =        
        let memMonoisoMass =
            Memoization.memoizeP (BioItem.formula >> Formula.monoisoMass)
        (fun bs -> 
            bs 
            |> Array.sumBy memMonoisoMass)

    /// Returns a function to calculate the average mass of the given sequence !memoization
    let initAverageMass<'a when 'a :> IBioItem> : (BioArray<_> -> float) =
        let memAverageMass =
            Memoization.memoizeP (BioItem.formula >> Formula.averageMass)
        (fun bs -> 
            bs 
            |> Array.sumBy memAverageMass)

    /// Returns a function to calculate the monoisotopic mass of the given sequence and initial value (e.g. H2O) !memoization
    let initMonoisoMassWith<'a when 'a :> IBioItem> (state:float) : (BioArray<_> -> float)  =        
        let memMonoisoMass =
            Memoization.memoizeP (BioItem.formula >> Formula.monoisoMass)
        (fun bs -> 
            bs |> Array.fold (fun massAcc item -> massAcc + memMonoisoMass item) state)

    /// Returns a function to calculate the average mass of the given sequence and initial value (e.g. H2O) !memoization
    let initAverageMassWith<'a when 'a :> IBioItem> (state:float) : (BioArray<_> -> float) =
        let memAverageMass =
            Memoization.memoizeP (BioItem.formula >> Formula.averageMass)
        (fun bs -> 
            bs |> Array.fold (fun massAcc item -> massAcc + memAverageMass item) state)

    ///Creates an array with information about the abundacies of the distinct BioItems by converting the symbol of the BioItem to an integer and incrementing the given integer. To decrease the size of the resulting array by still having a fast performance, all indices are shifted by 65. Therefore to call the abundancy of a given BioItem, use "Resultcompositionvector.[(BioItem.symbol bioitem) - 65]"
    let toCompositionVector (input:BioArray<_>)  =
        let compVec = Array.zeroCreate 26
        input
        |> Array.iter (fun a ->                         
                            let index = (int (BioItem.symbol a)) - 65
                            if index >= 0 then compVec.[index] <- compVec.[index] + 1)
        compVec    

    ///Creates an array with information about the abundacies of the distinct BioItems by converting the symbol of the BioItem to an integer and incrementing the given integer. To decrease the size of the resulting array by still having a fast performance, all indices are shifted by 65. Therefore to call the abundancy of a given BioItem, use "Resultcompositionvector.[(BioItem.symbol bioitem) - 65]"
    let toRelCompositionVector (input:BioArray<_>)  =
        let cvec = toCompositionVector input
        let sum  = cvec |> Array.sum  |> float
        cvec |> Array.map (fun i -> float i / sum)

    let initSampleBy (rnd:System.Random) (compositionVector:int[]) =
        if compositionVector.Length < 26 then failwith "Amino acid composition vector must have length 26 "
        let normalize (arr:int[]) =
            let sum = arr |> Array.sum |> float
            arr |> Array.map (fun x -> float x / sum)
        let tmp =
            compositionVector
            |> normalize
            |> Array.mapi (fun i x -> i,x)
            |> Array.sortBy snd
        let indexArr = tmp |> Array.map fst 
        let valueArr = tmp |> Array.map snd |> Array.scan (fun s v -> s + v) 0.0 |> Array.tail
        fun () ->
            let rndv =  rnd.NextDouble()
            match Array.tryFindIndex (fun x -> x > rndv) valueArr with
            | Some index -> 
                indexArr.[index] + 65 |> AminoAcidSymbols.aminoAcidSymbol
            | None -> AminoAcidSymbols.AminoAcidSymbol.Gap

