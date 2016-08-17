namespace BioFSharp

module BioList =

    open FSharp.Care
    open BioFSharp.BioItemsConverter
    
    type BioList<[<EqualityConditionalOn; ComparisonConditionalOn >]'a when 'a :> IBioItem> = list<'a>

    /// Generates amino acid sequence of one-letter-code string using given OptionConverter
    let ofAminoAcidStringWithOptionConverter (converter:OptionConverter.AminoAcidOptionConverter) (s:#seq<char>) : BioList<_> =          
        s
        |> Seq.choose converter
        |> Seq.toList


    /// Generates amino acid sequence of one-letter-code raw string
    let ofAminoAcidString (s:#seq<char>) : BioList<_> =          
        s
        |> Seq.choose OptionConverter.charToOptionAminoAcid
        |> Seq.toList

    /// Generates nucleotide sequence of one-letter-code string using given OptionConverter
    let ofNucleotideStringWithOptionConverter (converter:OptionConverter.NucleotideOptionConverter) (s:#seq<char>) : BioList<_> =             
        s
        |> Seq.choose converter
        |> Seq.toList

    /// Generates nucleotide sequence of one-letter-code raw string
    let ofNucleotideString (s:#seq<char>) : BioList<_> =             
        s
        |> Seq.choose OptionConverter.charToOptionNucleotid           
        |> Seq.toList

    
//    /// Builts a new collection whose elements are the result of applying
//    /// the given function to each triplet of the collection. 
//    let mapInTriplets f (input:seq<'a>) =
//        let sourceIsEmpty = ref false    
//        seq {   use en = input.GetEnumerator()
//                while not(!sourceIsEmpty) do                
//                match en with
//                | Triplet t -> yield (f t)                                                              
//                | _         -> sourceIsEmpty := true                               
//        }

    //  Replace T by U
    /// Transcribe a given DNA coding strand (5'-----3')
    let transcribeCodeingStrand (nucs:BioList<Nucleotides.Nucleotide>) = 
        nucs |> List.map (fun nuc -> Nucleotides.replaceTbyU nuc)
        


    //  
    /// Transcribe a given DNA template strand (3'-----5')
    let transcribeTemplateStrand (nucs:BioList<Nucleotides.Nucleotide>) = 
        nucs |> List.map (fun nuc -> Nucleotides.replaceTbyU (Nucleotides.complement nuc))


    /// translates nucleotide sequence to aminoacid sequence    
    let translate (nucleotideOffset:int) (rnaSeq:BioList<Nucleotides.Nucleotide>) =         
        if (nucleotideOffset < 0) then
                raise (System.ArgumentException(sprintf "Input error: nucleotide offset of %i is invalid" nucleotideOffset))                
        rnaSeq
        |> List.skip nucleotideOffset
        // TODO:
        //|> mapInTriplets Nucleotides.lookupBytes

    
    /// Compares the elemens of two sequence
    let isEqual a b =
        List.compareWith 
            (fun elem1 elem2 ->
                if elem1 = elem2 then 0    
                else 1)  a b 
        
    /// Returns string of one-letter-code
    let toString (bs:BioList<_>) =
        new string (bs |> List.map BioItem.symbol |> List.toArray) 


       
    /// Returns formula
    let toFormula (bs:BioList<#IBioItem>) =
        bs |> List.fold (fun acc item -> Formula.add acc  (BioItem.formula item)) Formula.emptyFormula
        

    /// Returns monoisotopic mass of the given sequence
    let toMonoisotopicMass (bs:BioList<#IBioItem>) =
        bs |> List.sumBy BioItem.monoisoMass


    /// Returns average mass of the given sequence
    let toAverageMass (bs:BioList<#IBioItem>) =
        bs |> List.sumBy BioItem.averageMass


    /// Returns monoisotopic mass of the given sequence and initial value (e.g. H2O) 
    let toMonoisotopicMassWith (state) (bs:BioList<#IBioItem>) =
        bs |> List.fold (fun massAcc item -> massAcc + BioItem.monoisoMass item) state


    /// Returns average mass of the given sequence and initial value (e.g. H2O) 
    let toAverageMassWith (state) (bs:BioList<#IBioItem>) =
        bs |> List.fold (fun massAcc item -> massAcc + BioItem.averageMass item) state


    /// Returns a function to calculate the monoisotopic mass of the given sequence !memoization
    let initMonoisoMass<'a when 'a :> IBioItem> : (BioList<'a> -> float) =        
        let memMonoisoMass =
            Memoization.memoizeP (BioItem.formula >> Formula.monoisoMass)
        (fun bs -> 
            bs 
            |> List.sumBy memMonoisoMass)


    /// Returns a function to calculate the average mass of the given sequence !memoization
    let initAverageMass<'a when 'a :> IBioItem> : (BioList<'a> -> float) =
        let memAverageMass =
            Memoization.memoizeP (BioItem.formula >> Formula.averageMass)
        (fun bs -> 
            bs 
            |> List.sumBy memAverageMass)


    /// Returns a function to calculate the monoisotopic mass of the given sequence and initial value (e.g. H2O) !memoization
    let initMonoisoMassWith<'a when 'a :> IBioItem> (state:float) : (BioList<'a> -> float)  =        
        let memMonoisoMass =
            Memoization.memoizeP (BioItem.formula >> Formula.monoisoMass)
        (fun bs -> 
            bs |> List.fold (fun massAcc item -> massAcc + memMonoisoMass item) state)


    /// Returns a function to calculate the average mass of the given sequence and initial value (e.g. H2O) !memoization
    let initAverageMassWith<'a when 'a :> IBioItem> (state:float) : (BioList<'a> -> float) =
        let memAverageMass =
            Memoization.memoizeP (BioItem.formula >> Formula.averageMass)
        (fun bs -> 
            bs |> List.fold (fun massAcc item -> massAcc + memAverageMass item) state)


    let toCompositionVector (input:BioList<_>)  =
        let compVec = Array.zeroCreate 26
        input
        |> Seq.iter (fun a ->                         
                            let index = (int (BioItem.symbol a)) - 65
                            compVec.[index] <- compVec.[index] + 1)
        compVec   


