namespace BioFSharp

open System

///Marker interface for BioItem base.
//[<StructuralEquality;StructuralComparison>]
type IBioItem =    
    abstract member Symbol   : char    
    abstract member Formula  : Formula.Formula    
    abstract member isTerminator : bool         
    abstract member isGap        : bool    


/// Basic functions on IBioItems interface
module BioItem = 

    /// Returns then symbol of the bio item
    let symbol (bItem:#IBioItem) =
        bItem.Symbol


    /// Returns then byteCode of the bio item
    let formula  (bItem:#IBioItem) =
        bItem.Formula


///// Type abbreviation for converting char to Bioitem
//type bioItemConverter<'a when 'a :> IBioItem> = char -> 'a option
//
//
///// Type abbreviation for converting char to optional Bioitem
//type bioItemOptionConverter<'a when 'a :> IBioItem> = char -> 'a option
