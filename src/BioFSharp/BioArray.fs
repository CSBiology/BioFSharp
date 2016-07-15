namespace BioFSharp

module BioArray =
    
    open FSharp.Care
    open IBioSequence

    //type IBioSequence<[<EqualityConditionalOn; ComparisonConditionalOn >]'a when 'a :> IBioItem> =  seq<'a>
    type BioArray<[<EqualityConditionalOn; ComparisonConditionalOn >]'a when 'a :> IBioItem> = array<'a>


    /// Generates amino acid sequence of one-letter-code string using given OptionConverter
    let ofAminoAcidStringWithOptionConverter (converter:OptionConverter.AminoAcidOptionConverter) (s:string) : BioArray<_> =
        IBioSequence.ofAminoAcidStringWithOptionConverter converter s 
        |> Seq.toArray



    /// Generates amino acid sequence of one-letter-code raw string
    let ofAminoAcidString (s:#seq<char>) : BioArray<_> =
        IBioSequence.ofAminoAcidString s 
        |> Seq.toArray

    /// Generates nucleotide sequence of one-letter-code string using given OptionConverter
    let ofNucleotideStringWithOptionConverter (converter:OptionConverter.NucleotideOptionConverter) (s:#seq<char>) : BioArray<_> =
        IBioSequence.ofNucleotideStringWithOptionConverter converter s 
        |> Seq.toArray
        
    /// Generates nucleotide sequence of one-letter-code raw string
    let ofNucleotideString (s:#seq<char>) : BioArray<_> =
        IBioSequence.ofNucleotideString s 
        |> Seq.toArray


    /// Builts a new collection whose elements are the result of applying
    /// the given function to each triplet of the collection. 
    let mapInTriplets f (input:BioArray<'a>) =        
        Array.init (input.Length / 3) (fun i -> f (input.[i],input.[i+1],input.[i+2]) )
        

    //  Replace T by U
    /// Transcribe a given DNA coding strand (5'-----3')
    let transcribeCodeingStrand (nucs:BioArray<Nucleotides.Nucleotide>) : BioArray<_> = 
        nucs |> Array.map (fun nuc -> Nucleotides.replaceTbyU nuc)
        


    //  
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
        let tmp = Array.compareWith (fun elem1 elem2 ->
                                        if elem1 = elem2 then 0    
                                        else 1)  a b 
        tmp = 0



    /// Returns string of one-letter-code
    let toString (bs:BioArray<_>) =
        new string (bs |> Array.map BioItem.symbol) 


       
    /// Returns formula
    let toFormula (bs:BioArray<_>) =
        bs |> Array.fold (fun acc item -> Formula.add acc  (BioItem.formula item)) Formula.emptyFormula

    /// Returns monoisotopic mass of the given sequence !memoization
    let toMonoisotopicMass<'a when 'a :> IBioItem> : (seq<'a> -> float) =        
        let memMonoisoMass =
            Memoization.memoizeP (BioItem.formula >> Formula.monoisoMass)
        (fun bs -> 
            bs 
            |> Seq.sumBy memMonoisoMass)        

    /// Returns average mass of the given sequence !memoization
    let toAverageMass<'a when 'a :> IBioItem> : (BioArray<_> -> float) =
        let memAverageMass =
            Memoization.memoizeP (BioItem.formula >> Formula.averageMass)
        (fun bs -> 
            bs 
            |> Array.sumBy memAverageMass)


    /// Returns monoisotopic mass of the given sequence including water (+H20)
    let toMonoisotopicMassWithWater<'a when 'a :> IBioItem> : (BioArray<_> -> float) =
        let water = Formula.Table.H2O |> Formula.averageMass
        let memMonoisoMass =
            Memoization.memoizeP (BioItem.formula >> Formula.monoisoMass)
        (fun bs -> 
            bs 
            |> Array.sumBy memMonoisoMass
            |> (+) water )


    /// Returns average mass of the given sequence including water (+H20)
    let toAverageMassWithWater<'a when 'a :> IBioItem> : (BioArray<_> -> float) =
        let water = Formula.Table.H2O |> Formula.averageMass
        let memAverageMass =
            Memoization.memoizeP (BioItem.formula >> Formula.averageMass)
        (fun bs -> 
            bs 
            |> Array.sumBy memAverageMass
            |> (+) water )


    let toCompositionVector (input:BioArray<_>)  =
        let compVec = Array.zeroCreate 26
        input
        |> Array.iter (fun a ->                         
                            let index = (int (BioItem.symbol a)) - 65
                            compVec.[index] <- compVec.[index] + 1)
        compVec    