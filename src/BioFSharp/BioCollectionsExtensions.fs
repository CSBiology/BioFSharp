namespace BioFSharp

open BioSeq
open BioArray
open BioList

[<AutoOpen>]
module BioCollectionsExtensions = 
    module BioSeq =
    
        [<CompiledName("ToBioArray")>]
        let toBioArray  (bioSeq:BioSeq<_>) : BioArray<_> = Seq.toArray bioSeq    
        [<CompiledName("ToBioList")>]
        let toBioList   (bioSeq:BioSeq<_>) : BioList<_>  = Seq.toList  bioSeq

        [<CompiledName("OfBioArray")>]
        let ofBioArray  (bioArray:BioArray<_>) : BioSeq<_> = Seq.ofArray bioArray
        [<CompiledName("OfBioList")>]
        let ofBioList   (bioList :BioList<_>)  : BioSeq<_> = Seq.ofList  bioList

    module BioList =
    
        [<CompiledName("ToBioArray")>]
        let toBioArray  (bioList:BioList<_>) : BioArray<_> = List.toArray bioList
        [<CompiledName("ToBioSeq")>]
        let toBioSeq    (bioList:BioList<_>) : BioSeq<_>   = List.toSeq   bioList
        
        [<CompiledName("OfBioArray")>]
        let ofBioArray  (bioArray:BioArray<_>) : BioList<_> = List.ofArray bioArray                                     
        [<CompiledName("OfBioSeq")>]
        let ofBioSeq    (bioSeq:BioSeq<_>)     : BioList<_> = List.ofSeq   bioSeq

    module BioArray =
    
        [<CompiledName("ToBioList")>]
        let toBioList   (bioArray:BioArray<_>) : BioList<_> = Array.toList bioArray
        [<CompiledName("ToBioSeq")>]
        let toBioSeq    (bioArray:BioArray<_>) : BioSeq<_>  = Array.toSeq  bioArray

        [<CompiledName("OfBioList")>]
        let ofBioList   (bioList :BioList<_>)  : BioArray<_> = Array.ofList  bioList
        [<CompiledName("OfBioSeq")>]                    
        let ofBioSeq    (bioSeq  :BioSeq<_>)   : BioArray<_> = Array.ofSeq   bioSeq