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
    
    
    
    ///Separates every key-value pair of field 'attributes' at ';'. Seperates key from value at '=' and separates values at ','.
    let private innerTokenizer (attributes: string) = 
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
    let private addFastaSequence (en:IEnumerator<string>) (acc:GFF<'a> list) converter= 
        let seqLeft = 
            let rec loop (acc:string list) =
                if en.MoveNext() then
                    //printfn "%s" en.Current
                    //loop (Seq.append acc (seq [en.Current]))
                    loop (en.Current::acc)
                else 
                    acc 
                    |> List.rev
                    |> fun x -> FastA.fromFileEnumerator converter x
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
    
    
    

    //SO_Terms which can be chosen for the feature field. Only these SO_Terms are valid
    let private SO_Terms so_TermsPath=
        FileIO.readFile(so_TermsPath)
        |> Seq.head
        |> fun x -> x.Split([|' '|])
            
    //directives of GFF3
    let private directives = 
        [|"##gff-version"; "##sequence-region"; "##feature-ontology";
        "##attribute-ontology"; "##source-ontology"; "##species"; 
        "##genome-build"; "###"|]
    
    //Validates GFF3 file. Prints first appearance of an error with line index. If needed an SO(FA) check is possible
    let sanityCheckWithSOTerm so_TermsPath filepath =
        let strEnumerator = (FileIO.readFile(filepath)).GetEnumerator()
        let so_TermsPathEmpty = 
            if so_TermsPath = "" then true else false 
        if strEnumerator.MoveNext() then
            //every gff3 file has to start with "##gff-version 3[...]"
            match strEnumerator.Current.StartsWith("##gff-version 3") || strEnumerator.Current.StartsWith("##gff-version\t3") || strEnumerator.Current.StartsWith("##gff-version   3") with
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
                                    | true  -> printfn "If no errors are printed above, this is a valid GFF3 file untill ##FASTA directive appears."
                                    | false ->
                                        match directives |> Array.tryFind (fun x -> x = strEnumerator.Current.Split([|' '|]).[0]) with
                                        | Some x -> loop (i+1)
                                        | _ -> 
                                            printfn "Wrong directive in line %i" i 
                                            loop (i+1)
                                | _   -> loop (i+1)
                            | '>' -> 
                                printfn "Unexpected '>' at the first postition of line %i. '>' at first position of a line is forbidden before the ##FASTA directive!" i
                                loop (i+1)
                            | _ ->
                                let splitstr = strEnumerator.Current.Split([|'\t'|])
                                match splitstr.Length<9 with
                                | false -> 
                                    match splitstr.[0].Contains(" ") with
                                    | true -> 
                                        printfn "Field seqid must not contain whitespace in line %i!" i
                                        loop (i+1)
                                    | false -> 
                                            let attributesSubStr = splitstr.[8].Split([|'=';';'|])
                                            match attributesSubStr.Length%2 with
                                            | 0 -> 
                                                match splitstr.[2] with
                                                | "CDS" -> 
                                                    match splitstr.[7] with
                                                    | "" -> 
                                                        printfn "Empty colum 8 in line %i!" i
                                                        loop (i+1)
                                                    | "." -> 
                                                        printfn "At CDS features the phase (column 8) is missing in line %i!" i
                                                        loop (i+1)
                                                    | _ -> loop (i+1)
                                                | _ ->
                                                    if so_TermsPathEmpty then 
                                                        loop (i+1) 
                                                    else 
                                                        match (SO_Terms so_TermsPath) |> Array.tryFind (fun x -> x= splitstr.[2]) with                              //eclude these to ignore Sequence Ontology terms
                                                        | None ->                                                                                                   //eclude these to ignore Sequence Ontology terms
                                                            printfn "In line %i coloum 3 should contain SO-Name or SOFA-ID. %s is not part of it" i splitstr.[2]    //eclude these to ignore Sequence Ontology terms
                                                            loop (i+1)                                                                                              //eclude these to ignore Sequence Ontology terms    
                                                        | Some x ->                                                                                                 //eclude these to ignore Sequence Ontology terms
                                                            loop (i+1) 
                                            | _ -> 
                                                printfn "Error in field 9 (attributes) in line %i. ['=';';'] are reserved for separation purposes and are not allowed in the key or value!" i
                                                loop (i+1)
                                | true -> 
                                    printfn "Wrong column filling. The number of fields in row %i is not 9!" i
                                    loop (i+1)
                    else printfn "If no errors are printed above, this is a valid GFF3 file!"
                loop 2

    let sanityCheck filepath = sanityCheckWithSOTerm "" filepath
    
    
    
    
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
                elif x.Attributes.ContainsKey("Id") then 
                    let IDlist = x.Attributes.["Id"]
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
  

    
    let gFFEntrytoString (g: GFFEntry) =
        let toStringMap (m: Map<string,string list>)= 
            m
            |> Seq.map (fun entry -> entry.Key,String.concat "," entry.Value)
            |> Seq.map (fun (k,v) -> sprintf "%s=%s" k v)
            |> String.concat ";"
        let toStringSup s=  
            String.concat "\t" s 
        let toStringChar c=
            match c with
            |'\u0000' -> "."
            | _ -> c |> string
        let toStringInt i=
            match i with
            | -1 -> "."
            | _ -> i |> string
        let toStringFloat f=
            let preC = f |> string
            match preC with
            | "NaN" -> "."
            | _ -> preC
        if g.Supplement.[0] = "No supplement" then 
            sprintf  "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s" g.Seqid g.Source g.Feature (toStringInt g.StartPos) (toStringInt g.EndPos) (toStringFloat g.Score) (toStringChar g.Strand) (toStringInt g.Phase) (toStringMap g.Attributes)
        else sprintf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s" g.Seqid g.Source g.Feature (toStringInt g.StartPos) (toStringInt g.EndPos) (toStringFloat g.Score) (toStringChar g.Strand) (toStringInt g.Phase) (toStringMap g.Attributes) (toStringSup g.Supplement)


    let GFF3Writer (input : seq<GFF<#seq<'a>>>) converter path=
        let en = input.GetEnumerator()
        let toString =
            seq {   
                while en.MoveNext() do
                        match en.Current with
                        | GFFLine (x)      ->   yield gFFEntrytoString x           
                        | Comment (x)      ->   yield x             
                        | Directive (x)    ->   yield x            
                        | Fasta (x)        ->   yield "##FASTA"    
                                                yield! FastA.toString converter x
                }
        toString
        |> Seq.writeOrAppend path
        printfn "Writing is finished! Path: %s" path