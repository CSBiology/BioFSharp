namespace BioFSharp

///This module contains the BioList type and its according functions. The BioList type is a List of objects using the IBioItem interface
module BioList =

    open System
    open FSharp.Care
    open BioFSharp.BioItemsConverter
    open AminoAcids
    open ModificationInfo
    open GlobalModificationInfo

    ///List of objects using the IBioItem interface
    type BioList<[<EqualityConditionalOn; ComparisonConditionalOn >]'a when 'a :> IBioItem> = list<'a>

    /// Generates amino acid sequence of one-letter-code string using given OptionConverter
    let ofAminoAcidStringWithOptionConverter (converter:OptionConverter.AminoAcidOptionConverter) (s:#seq<char>) : BioList<_> =          
        s
        |> Seq.choose converter
        |> Seq.toList

    /// Generates amino acid sequence of one-letter-code string containing modified AminoAcids indicated by 2 lowercase digits per modification. 
    let ofRevModAminoAcidString (converter: OptionConverter.AminoAcidOptionConverter) (converter': 'b -> Modification ) (xModToSearchMod: Map<string,'b>) (aaStr: string) : BioList<_>  =
        let aaStrL = aaStr.Length
        let rec loop count (modAcc: 'b list) acc (converter:OptionConverter.AminoAcidOptionConverter) (xModToSearchMod: Map<string,'b>)  (aaStr: string) = 
            if count = aaStrL then 
                 acc
            else 
                 let currentC = aaStr.[count]
                 if  (currentC |> Char.IsUpper = true) && modAcc = [] then 
                     loop (count+1) modAcc ((converter currentC).Value::acc) converter xModToSearchMod aaStr 
                 elif 
                     ((currentC |> Char.IsUpper) = true) then
                     let modList =
                           List.map converter' modAcc
                     let tmpAa = setModifications modList (converter currentC).Value
                     loop (count+1) [] (tmpAa::acc) converter xModToSearchMod aaStr
                 else 
                     match Map.tryFind aaStr.[count.. count+1] xModToSearchMod with
                     | Some modi -> loop (count+1) (modi::modAcc) acc converter xModToSearchMod aaStr
                     | None      -> loop (count+1) modAcc acc converter xModToSearchMod aaStr 
        
        loop 0 [] [] converter xModToSearchMod aaStr


    /// Generates amino acid sequence of one-letter-code string containing modified AminoAcids indicated by 2 lowercase digits per modification. 
    let ofRevModAminoAcidStringWithIsoMod (converter: OptionConverter.AminoAcidOptionConverter) (converter': 'b -> Modification ) (isotopMod: Modification list option) (xModToSearchMod: Map<string,'b>) (aaStr: string) : BioList<_>  =
        let aaStrL = aaStr.Length
        let rec loopWithGlobal count (modAcc: 'b list) acc (converter:OptionConverter.AminoAcidOptionConverter) (xModToSearchMod: Map<string,'b>)  (aaStr: string) = 
            if count = aaStrL then 
                 acc
            else 
                 let currentC = aaStr.[count]
                 if  (currentC |> Char.IsUpper = true) && modAcc = [] then 
                     let currentA = (converter currentC).Value 
                                    |> setModifications isotopMod.Value
                     loopWithGlobal (count+1) [] (currentA::acc) converter xModToSearchMod aaStr 
                 elif 
                     ((currentC |> Char.IsUpper) = true) then
                     let modList = List.map converter' (modAcc)
                     let tmpAa = setModifications (isotopMod.Value@modList) (converter currentC).Value
                     loopWithGlobal (count+1) [] (tmpAa::acc) converter xModToSearchMod aaStr
                 else 
                     match Map.tryFind aaStr.[count.. count+1] xModToSearchMod with
                     | Some modi -> loopWithGlobal (count+1) (modi::modAcc) acc converter xModToSearchMod aaStr
                     | None      -> loopWithGlobal (count+1) modAcc acc converter xModToSearchMod aaStr                 
        match isotopMod.IsSome with
        | true  -> loopWithGlobal 0 [] [] converter xModToSearchMod aaStr
        | false -> ofRevModAminoAcidString converter converter' xModToSearchMod aaStr

        
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

    ///Creates an array with information about the abundacies of the distinct BioItems by converting the symbol of the BioItem to an integer and incrementing the given integer. To decrease the size of the resulting array by still having a fast performance, all indices are shifted by 65. Therefore to call the abundancy of a given BioItem, use "Resultcompositionvector.[(BioItem.symbol bioitem) - 65]"
    let toCompositionVector (input:BioList<_>)  =
        let compVec = Array.zeroCreate 26
        input
        |> Seq.iter (fun a ->                         
                            let index = (int (BioItem.symbol a)) - 65
                            compVec.[index] <- compVec.[index] + 1)
        compVec   


