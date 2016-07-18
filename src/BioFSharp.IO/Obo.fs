namespace BioFSharp.IO


/// Module to parse obo files to AdjacencyGraph
module Obo =
    open System
    open FSharp.Care
    open FSharp.Care.IO


    /// obo term record type
    type OboTerm = {
            Id : string 
            Name : string 
            Namespace : string //new 
            Definition : string 
            Relationship : string
            Related_synonym : string list
            IsA  : string list // new
            Synonym : string list
            ExactSynonym : string
            BroadSynonym : string
            NarrowSynonym : string
            XrefAnalog : string
            Comment : string
            IsObsolete : string
            Replacedby : string //new
            Consider : string //new
            AltId : string
            DisjointFrom : string list
            Subset : string list
            IntersectionOf : string list
            Xref : string list
            PropertyValue : string
    }

    /// Creates an obo term record
    let createOboTerm id name nameSpace definition relationship related_synonym isA synonym 
            exactSynonym broadSynonym narrowSynonym xrefAnalog comment isObsolete replaced_by consider altId disjointFrom 
            subset intersectionOf xref propertyValue =
        { Id = id; Name = name;  Namespace = nameSpace; Definition = definition; Relationship = relationship; Related_synonym = related_synonym; IsA = isA;
            Synonym = synonym; ExactSynonym = exactSynonym; BroadSynonym = broadSynonym; NarrowSynonym = narrowSynonym;
            XrefAnalog = xrefAnalog; Comment = comment; IsObsolete = isObsolete; Replacedby = replaced_by; Consider = consider; AltId = altId; DisjointFrom = disjointFrom;
            Subset = subset; IntersectionOf = intersectionOf; Xref = xref; PropertyValue = propertyValue }

    type OboTermDef = {
        Id           : string
        Name         : string
        IsTransitive : string
        IsCyclic     : string
    }

    let createOboTermDef id name  isTransitive isCyclic =
        {Id = id; Name = name; IsTransitive = isTransitive; IsCyclic = isCyclic}



    /// Parses a [term] item in a recusive function
    let rec private parseSingleOboTerm (en:Collections.Generic.IEnumerator<string>) id name name_space definition relationship related_synonym isA synonym 
            exactSynonym broadSynonym narrowSynonym xrefAnalog comment isObsolete replaced_by consider altId disjointFrom subset intersectionOf xref propertyValue =     
        if en.MoveNext() then                
            let split = en.Current.Split([|": "|], System.StringSplitOptions.None)
            match split.[0] with
            | "id"              -> parseSingleOboTerm en split.[1] name name_space definition relationship related_synonym isA synonym exactSynonym broadSynonym narrowSynonym
                                     xrefAnalog comment isObsolete replaced_by consider altId disjointFrom subset intersectionOf xref propertyValue
        
            | "name"            -> parseSingleOboTerm en  id split.[1] name_space definition relationship related_synonym isA synonym exactSynonym broadSynonym narrowSynonym
                                     xrefAnalog comment isObsolete replaced_by consider altId disjointFrom subset intersectionOf xref propertyValue

            | "namespace"        -> parseSingleOboTerm en  id name split.[1] definition relationship related_synonym isA synonym exactSynonym broadSynonym narrowSynonym
                                     xrefAnalog comment isObsolete replaced_by consider altId disjointFrom subset intersectionOf xref propertyValue
        
            | "def"             -> parseSingleOboTerm en id name name_space split.[1] relationship related_synonym isA synonym exactSynonym broadSynonym narrowSynonym
                                     xrefAnalog comment isObsolete replaced_by consider altId disjointFrom subset intersectionOf xref propertyValue 
        
            | "relationship"    -> parseSingleOboTerm en  id name name_space definition split.[1] related_synonym isA synonym exactSynonym broadSynonym narrowSynonym
                                     xrefAnalog comment isObsolete replaced_by consider altId disjointFrom subset intersectionOf xref propertyValue
        
            | "is_a"            -> let tmp = split.[1].Split([|" !"|], System.StringSplitOptions.None)
                                   parseSingleOboTerm en  id name name_space definition relationship related_synonym (tmp.[0]::isA) synonym exactSynonym broadSynonym narrowSynonym
                                     xrefAnalog comment isObsolete replaced_by consider altId disjointFrom subset intersectionOf xref propertyValue
        
            | "related_synonym" -> parseSingleOboTerm en  id name name_space definition relationship (split.[1]::related_synonym) isA synonym exactSynonym broadSynonym narrowSynonym
                                     xrefAnalog comment isObsolete replaced_by consider altId disjointFrom subset intersectionOf xref propertyValue
        
            | "synonym"         -> parseSingleOboTerm en  id name name_space definition relationship related_synonym isA (split.[1]::synonym) exactSynonym broadSynonym narrowSynonym
                                     xrefAnalog comment isObsolete replaced_by consider altId disjointFrom subset intersectionOf xref propertyValue
        
            | "exact_synonym"   -> parseSingleOboTerm en  id name name_space definition relationship related_synonym isA synonym split.[1] broadSynonym narrowSynonym
                                     xrefAnalog comment isObsolete replaced_by consider altId disjointFrom subset intersectionOf xref propertyValue
        
            | "broad_synonym"   -> parseSingleOboTerm en  id name name_space definition relationship related_synonym isA synonym exactSynonym split.[1] narrowSynonym
                                     xrefAnalog comment isObsolete replaced_by consider altId disjointFrom subset intersectionOf xref propertyValue
        
            | "narrow_synonym"  -> parseSingleOboTerm en  id name name_space definition relationship related_synonym isA synonym exactSynonym broadSynonym split.[1]
                                     xrefAnalog comment isObsolete replaced_by consider altId disjointFrom subset intersectionOf xref propertyValue
        
            | "xref_analog"     -> parseSingleOboTerm en  id name name_space definition relationship related_synonym isA synonym exactSynonym broadSynonym narrowSynonym
                                     split.[1] comment isObsolete replaced_by consider altId disjointFrom subset intersectionOf xref propertyValue
        
            | "comment"         -> parseSingleOboTerm en  id name name_space definition relationship related_synonym isA synonym exactSynonym broadSynonym narrowSynonym
                                     xrefAnalog split.[1] isObsolete replaced_by consider altId disjointFrom subset intersectionOf xref propertyValue
        
            | "is_obsolete"     -> parseSingleOboTerm en  id name name_space definition relationship related_synonym isA synonym exactSynonym broadSynonym narrowSynonym
                                     xrefAnalog comment split.[1] replaced_by altId consider disjointFrom subset intersectionOf xref propertyValue
                                         
            | "replaced_by"     -> parseSingleOboTerm en  id name name_space definition relationship related_synonym isA synonym exactSynonym broadSynonym narrowSynonym
                                     xrefAnalog comment isObsolete split.[1] consider altId disjointFrom subset intersectionOf xref propertyValue

            | "consider"        -> parseSingleOboTerm en  id name name_space definition relationship related_synonym isA synonym exactSynonym broadSynonym narrowSynonym
                                     xrefAnalog comment isObsolete replaced_by split.[1] altId disjointFrom subset intersectionOf xref propertyValue
        
            | "alt_id"          -> parseSingleOboTerm en  id name name_space definition relationship related_synonym isA synonym exactSynonym broadSynonym narrowSynonym
                                     xrefAnalog comment isObsolete replaced_by consider split.[1] disjointFrom subset intersectionOf xref propertyValue
        
            | "disjoint_from"   -> parseSingleOboTerm en  id name name_space definition relationship related_synonym isA synonym exactSynonym broadSynonym narrowSynonym
                                     xrefAnalog comment isObsolete replaced_by consider altId (split.[1]::disjointFrom) subset intersectionOf xref propertyValue
        
            | "subset"          -> parseSingleOboTerm en  id name name_space definition relationship related_synonym isA synonym exactSynonym broadSynonym narrowSynonym
                                     xrefAnalog comment isObsolete replaced_by consider altId disjointFrom (split.[1]::subset) intersectionOf xref propertyValue
        
            | "intersection_of" -> parseSingleOboTerm en  id name name_space definition relationship related_synonym isA synonym exactSynonym broadSynonym narrowSynonym
                                     xrefAnalog comment isObsolete replaced_by consider altId disjointFrom subset (split.[1]::intersectionOf) xref propertyValue
        
            | "xref"            -> parseSingleOboTerm en  id name name_space definition relationship related_synonym isA synonym exactSynonym broadSynonym narrowSynonym
                                     xrefAnalog comment isObsolete replaced_by consider altId disjointFrom subset intersectionOf (split.[1]::xref) propertyValue
        
            | "property_value"  -> parseSingleOboTerm en  id name name_space definition relationship related_synonym isA synonym exactSynonym broadSynonym narrowSynonym
                                     xrefAnalog comment isObsolete replaced_by consider altId disjointFrom subset intersectionOf xref split.[1]
        
            | ""                -> createOboTerm id name name_space definition relationship related_synonym isA synonym exactSynonym broadSynonym narrowSynonym
                                     xrefAnalog comment isObsolete replaced_by consider altId disjointFrom subset intersectionOf xref propertyValue  
                                        
            | _                 -> parseSingleOboTerm en  id name name_space definition relationship related_synonym isA synonym exactSynonym broadSynonym narrowSynonym
                                     xrefAnalog comment isObsolete replaced_by consider altId disjointFrom subset intersectionOf xref propertyValue
        else
            failwithf "Unexcpected end of file."

    //parseTermDef
    let rec private parseSingleTermDef (en:Collections.Generic.IEnumerator<string>) id name isTransitive isCyclic =     
        if en.MoveNext() then                
            let split = en.Current.Split([|": "|], System.StringSplitOptions.None)
            match split.[0] with
            | "id"            -> parseSingleTermDef en split.[1] name isTransitive isCyclic
            | "name"          -> parseSingleTermDef en id split.[1] isTransitive isCyclic
        
        
            | "is_transitive" -> parseSingleTermDef en id name split.[1] isCyclic
            | "is_cyclic"     -> parseSingleTermDef en id name isTransitive split.[1]
            | ""              -> createOboTermDef id name isTransitive isCyclic
                      
            | _               -> parseSingleTermDef en id name isTransitive isCyclic
        else
            failwithf "Unexcpected end of file."


    /// Parse Obo Terms [Term] from seq<string>
    let parseOboTerms (input:seq<string>) =         
        let en = input.GetEnumerator()
        let rec loop (en:System.Collections.Generic.IEnumerator<string>) =
            seq {
                match en.MoveNext() with
                | true ->             
                    match en.Current with
                    | "[Term]"    -> yield (parseSingleOboTerm en "" "" "" "" "" [] [] [] "" "" "" "" "" "" "" "" "" [] [] [] [] "")
                                     yield! loop en 
                    | _ -> yield! loop en
                | false -> ()
            }
        loop en 


//    //########################################
//    // Definition of OboGraph
//
//
//    module FastOboGraph =
//
//        /// Obo Term as node
//        [<StructuredFormatDisplay("{PrettyString}")>]
//        type OboNode = { 
//            Id : int
//            Name : string
//            NameSpace : string
//            OntologyId : string // GO:...
//            }
//            with
//            member this.PrettyString = sprintf "%s:%07i | %s {%s}" this.OntologyId this.Id this.Name this.NameSpace
//            interface INode<int>
//                with member this.Id = this.Id                
//
//
//        /// Creates OboNode
//        let createOboNode id name nameSpace ontologyId =
//            {Id = id; Name = name; NameSpace = nameSpace; OntologyId = ontologyId; }
//
//
//
//        type OboEdgeType =
//            | Is_A
//            | Part_Of
//
//        [<StructuredFormatDisplay("{PrettyString}")>]
//        type OboEdge = { 
//            Id : int
//            SourceId :int
//            TargetId :int } 
//            with
//            member this.PrettyString =  if this.Id = this.SourceId then
//                                            sprintf "o---> %07i | (%i)" this.Id this.TargetId
//                                        else 
//                                            sprintf "%07i <---o | (%i)" this.Id this.TargetId
//            interface IEdge<int> with
//                member this.Id = this.Id
//                member this.SourceId = this.SourceId
//                member this.TargetId = this.TargetId
//            
//
//        /// Creates OboEdge
//        let createOboEdge id sourceId targetId =
//            {Id = id; SourceId = sourceId; TargetId = targetId}
//
//
//        type oboAdjacencyNode = AdjacencyNode<OboNode,OboEdge,int>
//
//
//
//        /// Splits String s at ":", returns sa.[1]
//        let tryIdToInt str =
//            match str with
//            | Regex.RegexValue @"GO:(?<goId>[\d]+)" [ goId; ] -> Some( int goId )
//            | _ -> None
//
//        let idToInt str =
//            match tryIdToInt str with
//            | Some v -> v
//            | None   -> failwithf "%s invaild GO id" str
//
//        let private oboIdStringToInt s =
//            let sa = String.split ':' s
//            if sa.Length > 1 then
//                sa.[1] |> int
//            else
//                -1
//
//        /// Creates fromOboTerm from oboTerm startIndex
//        let fromOboTerm (obo: OboTerm) (startIndex: int) =
//            let nodeId = oboIdStringToInt obo.Id
//            let node   = createOboNode nodeId obo.Name obo.Namespace
//            let edges = 
//                obo.IsA
//                |> List.mapi (fun i edId -> let edgeTargetId = oboIdStringToInt edId
//                                            createOboEdge (i+startIndex) nodeId edgeTargetId
//                              )
//            (node,edges,(startIndex + obo.IsA.Length))
//
//
//        /// Creates OboEnumerator from oboNode oboEdge
//        let oboTermToOboGraph (input: seq<OboTerm>) = //: seq<oboAdjacencyNode> =
//            let en = input.GetEnumerator()
//            let rec loop (en:System.Collections.Generic.IEnumerator<OboTerm>) acc  =
//                seq { 
//                    match en.MoveNext() with
//                    | true -> let cNode,cEdges,cIndex = fromOboTerm en.Current acc
//                      
//                              yield (cNode,cEdges)
//                              yield! loop en cIndex
//                    | false -> ()
//                    }
//            loop en 0
//
//
//        /// Reads obo file 
//        let readFile path =
//            FileIO.readFile path
//            |> parseOboTerms
//            |> oboTermToOboGraph
//            |> Seq.toList


