namespace BioFSharp


module GlobalModificationInfo =    
    
    open FSharp.Care
    open FSharp.Care.Collections
    open System.Text.RegularExpressions

    let REGEX_GLMOD_SYM = new Regex(@"(?<bioitem>[A-Za-z]):(?<mass>[-+]?\d*(.\d+)?)", RegexOptions.Compiled);


    type GlobalModification<'a when 'a :> IBioItem>(name,modList:seq<'a*float>,defaultValue) =
        let dict = 
            modList
            |> Seq.map (fun (k,v) -> BioItem.symbol k,v)
            |> Dict.ofSeq
            

        member x.Name = name
        member this.DefaultValue = defaultValue
        member this.Modifiy (bItem:IBioItem) =
            let symbol = BioItem.symbol bItem
            if dict.ContainsKey(symbol) then
                dict.[symbol]
            else
                defaultValue

        override x.ToString() =
            sprintf "%s|%s" x.Name 
                (dict 
                 |> Seq.map (fun kv -> sprintf "%c:%f" kv.Key kv.Value) |> String.concat "")

        override x.Equals(yobj) =
            match yobj with
            | :? GlobalModification<'a> as y -> (x.Name = x.Name)
            | _ -> false
 
        override x.GetHashCode() = hash x.Name
                        
        interface System.IComparable with
            member x.CompareTo yobj =
                match yobj with
                | :? GlobalModification<IBioItem> as y -> compare x.Name y.Name
                | _ -> invalidArg "yobj" "cannot compare values of different types"







    let ofModAminoAcids name (massFunction:AminoAcids.AminoAcid -> float) (modAminos:seq<AminoAcids.AminoAcid>) =
        let modList =
            modAminos
            |> Seq.map (fun aa -> 
                            let modMass = massFunction aa 
                            let rawMass = 
                                match aa with
                                | AminoAcids.Mod (a,_) ->  massFunction a
                                | _           -> 0.0
                            (aa,modMass - rawMass)               
                       )
        GlobalModification(name,modList,0.0)

    let toString (gm:GlobalModification<_>) =
        gm.ToString()



    let ofString (parse:char->'a) (str:string) =
        let splitStr = str.Split('|')
        let name = splitStr.[0]
        let matches = REGEX_GLMOD_SYM.Matches (splitStr.[1])
        // regex results as seq
        let ms = seq {for i = 0 to matches.Count - 1 do yield matches.[i]}
        let modList = 
            ms
            |> Seq.map ( fun g ->             
                let item = parse (g.Groups.["bioitem"].Value).[0]
                let mass = float (g.Groups.["mass"].Value)
                (item,mass)
                )
        GlobalModification<'a>(name,modList,0.0)
    

    module Table =     

        let N15 = 
            AminoAcids.AminoAcidSetStandard
            |> Seq.map (fun a -> a |> AminoAcids.setModification ModificationInfo.Table.N15)
            |> Seq.toList
            |> ofModAminoAcids "#N15" AminoAcids.monoisoMass 




