namespace BioFSharp

module GlobalModificationInfo =    
        
    
    type GlobalAAModificator = AminoAcids.AminoAcid -> float

    /// Returns a function that calculates the difference between the unmodified and the isotopically modified version of a given amino acid.
    let initGlobalModificationDeltaOfAA (massfunction: IBioItem -> float) (isotopModifications:ModificationInfo.Modification list) : GlobalAAModificator =
        let dictAA = new  System.Collections.Concurrent.ConcurrentDictionary<char,float>()
        let totalIsoMod = ModificationInfo.createModification "AccumulatedIsoMods" false ModificationInfo.ModLocation.Isotopic (fun f -> isotopModifications |> List.fold (fun modF isoMod -> isoMod.Modify modF) f)
        let massOfAminoAcid = 
            fun (b:AminoAcids.AminoAcid) ->
                let key = AminoAcids.symbol b
                match dictAA.TryGetValue(key) with
                | true,value -> value
                | false,_    ->
                    let massWith =
                        AminoAcids.setModification totalIsoMod b
                        |> massfunction 
                    let mass = massfunction b
                    dictAA.GetOrAdd(key,(massWith - mass))
        massOfAminoAcid

    type GlobalModModificator = ModificationInfo.Modification -> float

    /// Returns a function that calculates the difference between the unmodified and the isotopically modified version of a given modification
    let initGlobalModificationDeltaOfMod (massfunction: IBioItem -> float) (isotopModifications:ModificationInfo.Modification list) : GlobalModModificator =
        let dictMod = new  System.Collections.Concurrent.ConcurrentDictionary<string,float>()
        let totalIsoMod = ModificationInfo.createModification "AccumulatedIsoMods" false ModificationInfo.ModLocation.Isotopic (fun f -> isotopModifications |> List.fold (fun modF isoMod -> isoMod.Modify modF) f)
        let massOfAminoAcid = 
            fun (b:ModificationInfo.Modification) ->
                if not b.IsBiological then  
                    0.   
                else 
                let key = b.Name
                match dictMod.TryGetValue(key) with
                | true,value -> value
                | false,_    ->
                    let massWith =
                        {b with Name = b.Name + "_isoMod";Modify = (b.Modify >> totalIsoMod.Modify)} 
                        |> massfunction 
                    let mass = massfunction b
                    dictMod.GetOrAdd(key,(massWith - mass))
        massOfAminoAcid

