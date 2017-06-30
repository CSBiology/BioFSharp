namespace BioFSharp.IO
module GFF3Parser =


    open BioFSharp.IO
    open FSharp.Care.Regex
    open FSharp.Care.IO
    open System.Collections.Generic
    
    type GFFEntry = {
        Seqid       : string
        Source      : string
        Feature     : string
        StartPos    : int
        EndPos      : int
        Score       : float
        Strand      : char
        Phase       : int
        Attributes  : Map<string,(string list)>
        Supplement  : string [] 
                   }
    
    type GFF<'a>  =
    | GFFLine         of GFFEntry
    | Comment         of string
    | Directive       of string
    | Fasta           of seq<FastA.FastaItem<'a>>
    
    
    module Reader =
        ///Separates every key-value pair of field 'attributes' at ';'. Seperates key from value at '=' and separates values at ','.
        let innerTokenizer (attributes: string) = 
            //Regex for separating key and values
            let tryMatchKeyValues str =
                match str with
                    | RegexGroups @"(?<key>[^=]+)={1}((?<values>[^,]+),?)*" g -> 
                        let tmp =
                            g |> List.map (fun a -> a.["key"].Value, [for c in a.["values"].Captures -> c.Value]) |> List.head
                        Some tmp
                    | _ -> None
    
            attributes.Split([|';'|])
            |> Seq.choose tryMatchKeyValues
            |> Map.ofSeq
    
        ///Gets strings of each field and creates a GFFEntry type thereby converting the strings into desired types.        
        let createGFFEntry seqid source feature startPos endPos score strand phase attributes (supplement:string) =
            let parseStrToFloat (str:string) =
                let defaultFloat = nan
                match str with
                | "." -> defaultFloat
                | ""  -> defaultFloat
                | _ -> str |> float
    
            let parseStrToInt (str:string) =  
                let defaultInt = -1
                match str with
                | "." -> defaultInt
                | ""  -> defaultInt
                | _ -> str |> int    
    
            let parseStrToChar (str:string) = 
                let defaultChar = '\u0000'
                match str with
                | "." -> defaultChar
                | ""  -> defaultChar
                | _ -> str |> char
    
            let parseStrToMap (str:string) =
                let defaultMap = Map.empty
                match str with
                | "." -> defaultMap
                | ""  -> defaultMap
                | _ -> str |> innerTokenizer 
    
            {
            Seqid       = seqid
            Source      = source
            Feature     = feature
            StartPos    = startPos      |> parseStrToInt
            EndPos      = endPos        |> parseStrToInt
            Score       = score         |> parseStrToFloat
            Strand      = strand        |> parseStrToChar
            Phase       = phase         |> parseStrToInt
            Attributes  = attributes    |> parseStrToMap
            Supplement  = supplement    |> fun x -> x.Split([|'\t'|])
            }
    
    
        ///Converts a string into a GFFEntry type. If there are more than 9 fields an additional "supplement" field gets filled. If there are less than 9 only the supplement field gets filled with the string.
        let parseStrToGFFEntry (str:string) =
            //splits the string at tap('\t') in <= 10 substrings 
            let split = str.Split([|'\t'|],10)
            //counts the number of entries to validate the GFF line
            let columNumber = split.Length
            if columNumber <= 8 then 
                 createGFFEntry ""        ""        ""        ""        ""        ""        ""        ""        ""        ("wrong format: " + str)
            elif columNumber = 9 then
                 createGFFEntry split.[0] split.[1] split.[2] split.[3] split.[4] split.[5] split.[6] split.[7] split.[8] "No supplement"
            else createGFFEntry split.[0] split.[1] split.[2] split.[3] split.[4] split.[5] split.[6] split.[7] split.[8] split.[9] 
    
    
    
        ///If there is a ##FASTA directive, all subsequent lines are taken by this function, transformed to seq<FastA.FastaItem<'a>> and added to the previous parsed GFF<'a> list.
        let addFastaSequence (en:IEnumerator<string>) (acc:GFF<'a> list) converter= 
            let seqLeft = 
                let rec loop (acc:seq<string>) =
                    if en.MoveNext() then
                        loop (Seq.append acc (seq [en.Current]))
                    else acc |> FastA.fromFileEnumerator converter
                loop [en.Current]
            
            let finalSeq =
                List.append (acc |> List.rev) [(Fasta seqLeft)]
                |> Seq.cast
            finalSeq         
            
        ///reads in a file and gives a GFF<'a> list. If file contains a FastA sequence it is converted to FastA.FastaItem with given converter. (Use 'id' as converter if no FastA is required).
        let GFF3reader fastAconverter filepath =
            let strEnumerator = (FileIO.readFile(filepath)).GetEnumerator()
    
            //Converts every string in file into GFF type by using active pattern. If FastA sequence is included the IEnumerator and the until here parsed GFF<'a> list is transfered to 'addFastaSequence'.
            let rec parseFile (acc:GFF<'a> list) : seq<GFF<'a>>=
                if strEnumerator.MoveNext() then 
    
                    let (|Directive|_|) (str:string) =
                        if str.StartsWith("##") then Some str else None
                    let (|FastaDirective|_|) (str:string) =
                        if str.ToUpper().StartsWith("##FASTA") then Some str else None 
                    let (|BlankLine|_|) (str:string) =
                        if str.Length = 0 then Some str else None 
                    let (|Comment|_|) (str:string) =
                        if str.StartsWith("#") then Some str else None 
                       
                    let currentString = strEnumerator.Current
                    match currentString with
                    | FastaDirective s  -> addFastaSequence strEnumerator acc fastAconverter      
                    | Directive s       -> parseFile ((Directive s)::acc)
                    | Comment s         -> parseFile ((Comment s)::acc)
                    | BlankLine s       -> parseFile acc                    
                    | _                 -> parseFile ((GFFLine (currentString |> parseStrToGFFEntry))::acc) 
            
                else 
                    acc 
                    |> List.rev 
                    |> Seq.cast
            parseFile []
    
        ///If no information about Sequence is required or no Fasta is included you can use this function
        let GFF3readerWithoutFasta filepath = GFF3reader id filepath
    
    
    module Validator =

        //SO_Terms which can be chosen for the feature field. Only these SO_Terms are valid
        let SO_Terms so_TermsPath=
            FileIO.readFile(so_TermsPath)
            |> Seq.head
            |> fun x -> x.Split([|' '|])
            
        //directives of GFF3
        let directives = 
            [|"##gff-version"; "##sequence-region"; "##feature-ontology";
            "##attribute-ontology"; "##source-ontology"; "##species"; 
            "##genome-build"; "###"|]
    
        //Validates GFF3 file. Prints first appearance of an error with line index. If needed an SO(FA) check is possible
        let GFF3validator so_TermsPath filepath =
            let strEnumerator = (FileIO.readFile(filepath)).GetEnumerator()
            if strEnumerator.MoveNext() then
                //every gff3 file has to start with "##gff-version 3[...]"
                match strEnumerator.Current.StartsWith("##gff-version 3") || strEnumerator.Current.StartsWith("##gff-version\t3") with
                | false -> printfn "Error in first line. Should be ##gff-version 3[...]! with separation by one whitespace (no tab or '   ')"
                | true  ->
                    let rec loop i =
                        if strEnumerator.MoveNext() then 
                            //short strings are ignored!
                            if strEnumerator.Current.Length < 3 then 
                                loop (i+1)
                            else 
                                match strEnumerator.Current.[0] with
                                | '#' -> 
                                    match strEnumerator.Current.[1] with
                                    | '#' -> 
                                        match strEnumerator.Current.StartsWith("##FASTA") with
                                        | true  -> printfn "Valid GFF3 file untill ##FASTA directive appears."
                                        | false ->
                                            match directives |> Array.tryFind (fun x -> x = strEnumerator.Current.Split([|' '|]).[0]) with
                                            | Some x -> loop (i+1)
                                            | _ -> printfn "Wrong directive in line %i" i 
                                    | _   -> loop (i+1)
                                | '>' -> 
                                    printfn "Unexpected '>' at the first postition of line %i. '>' at first position of a line is forbidden before the ##FASTA directive!" i
                                | _ ->
                                    let splitstr = strEnumerator.Current.Split([|'\t'|])
                                    match splitstr.Length<9 with
                                    | false -> 
                                        match splitstr.[0].Contains(" ") with
                                        | true -> printfn "Field seqid must not contain whitespace in line %i!" i
                                        | false -> 
                                            match (SO_Terms so_TermsPath) |> Array.tryFind (fun x -> x= splitstr.[2]) with //eclude these to ignore Sequence Ontology terms
                                            | Some x ->                                                    //eclude these to ignore Sequence Ontology terms
                                                match splitstr.[2] with
                                                | "CDS" -> 
                                                    match splitstr.[7] with
                                                    | "" -> printfn "Empty colum 8 in line %i!" i
                                                    | "." -> printfn "At CDS features the phase (column 8) is missing in line %i!" i
                                                    | _ -> loop (i+1)
                                                | _ ->
                                                    let attributesSubStr = splitstr.[8].Split([|'=';';'|])
                                                    match attributesSubStr.Length%2 with
                                                    | 0 -> loop (i+1) 
                                                    | _ -> printfn "Error in field 9 (attributes) in line %i. ['=';';'] are reserved for separation purposes and are not allowed in the key/value!" i
                                            | _ -> printfn "In line %i coloum 3 should contain SO-Name or SOFA-ID" i //eclude these to ignore Sequence Ontology terms
                                    | true -> printfn "Wrong column filling. The number of fields in row %i is not 9!" i
                        else printfn "Valid GFF3 file!"
                    loop 2
    
    
    
    
    module Relationship = 
    
        ///Searches for an term and gives a list of all features of which the searchterm is the mainfeature (ID) or a child of it (Parent) (shows all features which are linked to searchterm)
        let relationshipSearch (gffList:seq<GFF<'a>>) searchterm = 
            let filteredGFFentries = 
                gffList 
                |> Seq.choose (fun x -> 
                    match x with
                    | GFFLine(s) -> Some s
                    | _ -> None)
            let parent   =  
                filteredGFFentries 
                |> Seq.filter (fun x -> 
                    if x.Attributes.ContainsKey("ID") then 
                        let IDlist = x.Attributes.["ID"]
                        let IDlistfilt = IDlist |> List.tryFind (fun x -> x.Contains(searchterm)) //x = searchterm
                        match IDlistfilt with
                        | Some x -> true
                        | _ -> false
                    else false)
            let child_of   = 
                filteredGFFentries
                |> Seq.filter (fun x -> 
                    if x.Attributes.ContainsKey("Parent") then 
                        let IDlist = x.Attributes.["Parent"]
                        let IDlistfilt = IDlist |> List.tryFind (fun x -> x.Contains(searchterm)) //x = searchterm
                        match IDlistfilt with
                        | Some x -> true
                        | _ -> false
                    else false)
            Seq.append parent child_of
  

//    module Writer =
//        
//        let GFF3Writer (input : seq<GFF<'a>>) :unit = 