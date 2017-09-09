namespace BioFSharp.Vis

// Set theory
module Venn = 

    // #########################################

    type VennSet<'l,'a when 'a : comparison> = {
        Label  : list<'l>
        Set    : Set<'a>
        }

    type GenericVenn<'l,'a when 'a : comparison> = Map<string,VennSet<'l,'a>>    

    
    /// Converts from label list to string id
    let labelToId (label:list<'l>) =
        label
        |> List.map (fun x -> x.ToString())
        |> List.toArray
        |> String.concat "&" 


    /// Generates a generic venn from a list of sets
    let ofSetList (labels:array<string>) (sets:array<Set<_>>) =    
        let union = Set.unionMany sets
    
        let toLabel (arr:bool[]) = 
            arr |> Seq.mapi (fun i b -> i,b) |> Seq.choose (fun (i,b) -> if b then Some(labels.[i]) else None) |> Seq.toList        
    
        union
        |> Seq.map (fun item ->  item,Array.init sets.Length (fun i -> sets.[i].Contains(item)))
        |> Seq.groupBy snd
        |> Seq.map (fun (k,v) -> let label =  toLabel k
                                 let value = (v |> Seq.map fst |> Set.ofSeq)
                                 (labelToId label),{Label = label; Set = value})
        |> Seq.append [("union",{Label = []; Set = union})]
        |> Map.ofSeq


    /// Converts a generic venn to the count venn
    let toVennCount (genericVenn:GenericVenn<_,_>) =   
        genericVenn
        |> Seq.map (fun (kv) -> kv.Key ,kv.Value.Set.Count)
        |> Map.ofSeq





// #########################################
// Chord connection for compatibility to Chord diagram

    type ChordConnections = {
        Group : int
        Value : int
    }

    let private chordConnectionsToJSON (c:ChordConnections) =
        sprintf "{\"group\":%i,\"value\":%i}" c.Group c.Value


    let toChordConnections (labels:array<'a>) (genericVenn:GenericVenn<'a,'b>) =     
        let f l = labels |> Array.findIndex (fun x -> x = l)
        genericVenn
        |> Seq.map (fun kv -> 
            kv.Value.Label 
            |> Seq.map (fun l -> { Group = f l; Value = kv.Value.Set.Count} )
                    )

    let chordConnectionsToString (d:seq<seq<ChordConnections>>) =
        d
        |> Seq.map (fun cl ->
            cl |> Seq.map (fun c -> chordConnectionsToJSON c) |> Seq.toArray |> String.concat "," 
                    )
        |> Seq.map (fun s -> sprintf "[%s]" s )
        |> Seq.toArray
        |> String.concat "," 
        |> fun s -> sprintf "[%s]" s




