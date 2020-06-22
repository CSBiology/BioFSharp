namespace BioFSharp

open BioSeq
open BioArray
open BioList

[<AutoOpen>]
module BioCollectionsExtensions = 
    module BioSeq =
        
        [<CompiledName("ToBioArray")>]
        /// Builds a BioArray from the given BioCollection.
        let toBioArray  (bioSeq:BioSeq<_>) : BioArray<_> = Seq.toArray bioSeq    
        [<CompiledName("ToBioList")>]
        /// Builds a BioList from the given BioCollection.
        let toBioList   (bioSeq:BioSeq<_>) : BioList<_>  = Seq.toList  bioSeq
        
        [<CompiledName("OfBioArray")>]
        ///Views the given BioArray as BioSeq.
        let ofBioArray  (bioArray:BioArray<_>) : BioSeq<_> = Seq.ofArray bioArray
        [<CompiledName("OfBioList")>]
        ///Views the given BioList as BioSeq.
        let ofBioList   (bioList :BioList<_>)  : BioSeq<_> = Seq.ofList  bioList

        [<CompiledName("Map")>]
        /// Builds a new BioSeq whose elements are the result of applying the given mapping function to each of the elements of the collection.
        let map (mapping: #IBioItem -> #IBioItem) (bioSeq:BioSeq<_>) : BioSeq<_> = bioSeq |> Seq.map mapping

    module BioList =
    
        [<CompiledName("ToBioArray")>]
        /// Builds a BioArray from the given BioList.
        let toBioArray  (bioList:BioList<_>) : BioArray<_> = List.toArray bioList
        [<CompiledName("ToBioSeq")>]
        ///Views the given BioList as BioSeq.
        let toBioSeq    (bioList:BioList<_>) : BioSeq<_>   = List.toSeq   bioList
        
        [<CompiledName("OfBioArray")>]
        /// Builds a BioList from the given BioArray.
        let ofBioArray  (bioArray:BioArray<_>) : BioList<_> = List.ofArray bioArray                                     
        [<CompiledName("OfBioSeq")>]
        /// Builds a BioList from the given BioSeq.
        let ofBioSeq    (bioSeq:BioSeq<_>)     : BioList<_> = List.ofSeq   bioSeq

        [<CompiledName("Map")>]
        /// Builds a new BioList whose elements are the result of applying the given mapping function to each of the elements of the collection.
        let map (mapping: #IBioItem -> #IBioItem) (bioList:BioList<_>) : BioList<_> = bioList |> List.map mapping

    module BioArray =
    
        [<CompiledName("ToBioList")>]
        /// Builds a BioList from the given BioArray.
        let toBioList   (bioArray:BioArray<_>) : BioList<_> = Array.toList bioArray
        [<CompiledName("ToBioSeq")>]
        ///Views the given BioArray as BioSeq.
        let toBioSeq    (bioArray:BioArray<_>) : BioSeq<_>  = Array.toSeq  bioArray

        [<CompiledName("OfBioList")>]
        /// Builds a BioArray from the given BioList.
        let ofBioList   (bioList :BioList<_>)  : BioArray<_> = Array.ofList  bioList
        [<CompiledName("OfBioSeq")>]                    
        /// Builds a BioArray from the given BioSeq.
        let ofBioSeq    (bioSeq  :BioSeq<_>)   : BioArray<_> = Array.ofSeq   bioSeq

        [<CompiledName("Map")>]
        /// Builds a new BioArray whose elements are the result of applying the given mapping function to each of the elements of the collection.
        let map (mapping: #IBioItem -> #IBioItem) (bioArray:BioArray<_>) : BioArray<_> = bioArray |> Array.map mapping