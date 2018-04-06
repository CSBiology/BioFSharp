namespace BioFSharp

module GlobalModificationInfo =    
        
    
    type GlobalModificator = AminoAcids.AminoAcid -> float

    let initGlobalModification (massfunction: IBioItem -> float) (isotopModification:ModificationInfo.Modification list) : GlobalModificator =
        let dict = new  System.Collections.Concurrent.ConcurrentDictionary<char,float>()
        fun (b:AminoAcids.AminoAcid) ->
            let key = AminoAcids.symbol b
            match dict.TryGetValue(key) with
            | true,value -> value
            | false,_    ->
                let massWith =
                    isotopModification
                    |> List.fold (fun acc x -> AminoAcids.setModification x b
                                               |> massfunction 
                                               |> (+) acc 
                                 ) 0.
                let mass = massfunction b
                dict.GetOrAdd(key,(massWith - mass))
 