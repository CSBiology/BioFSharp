namespace BioFSharp.IO

///functions for reading and writing GenBank(.gb) files
module GenBank =
      
    open System
    open FSharp.Care
    open FSharp.Care.IO
    open FSharp.Care.Regex
    open System.Collections.Generic
    open System.Text
    open FSharp.Care.Collections
    open FSharp.Care.String
    open BioFSharp
    open BioFSharp.IO
    
    
    ///Functions for reading a GenBank file
    ///Represents the possible sections in a GenBank file
    type private CurrentSection = 
        Meta = 1 |Reference = 2| Feature = 3 |Origin = 4 |End = 5

    ///Represents a single feature Qualifier and its value from the FEATURES section of a Genbank file. Features can contain
    ///Information about genes and gene products, as well as regions of biological significance reported in the sequence
    type FeatureQualifier = {
        ///Key of the Feature
        Name:string;
        ///Value of the Feature
        Value:string
        }
    let createFeatureQualifier name value = {Name=name;Value=value} 


    ///Represents a single feature from the FEATURES section of a GenBank file. Features can contain
    ///Information about genes and gene products, as well as regions of biological significance reported in the sequence
    type Feature = {
        ///Type of the Feature
        Type        : string;
        ///Location of the feature in the sequence
        BaseSpan    : string;
        ///A List of feature Qualifiers and their values associated with this feature
        Qualifiers  : FeatureQualifier list
    }
    let createFeature t bs qual = {Type = t; BaseSpan=bs; Qualifiers=qual}


    ///Represents any Item a GenBank file can contain as a union case. The result of parsing a genBank file will be a dictionary containing this type.
    type GenBankItem<'a> =
        ///Any value contained in the meta section of a GenBank file. 
        |Value of string
        ///All references contained in a GenBank file is seperate entries in a list.
        |References of (string*string) list list
        ///All features contained in a GenBank file as seperate entries in a list
        |Features of Feature list
        ///The origin section of a GenBank file
        |Sequence of 'a
    

    ///Iterate over an array yielding the result of applying a function to each element in a sequence
    let rec private arrayIteriYield (mapping: int -> 'a -> 'b) (a:array<'a>) =
        seq{
            for i=0 to a.Length-1 do
                yield mapping i a.[i]
            }
    
    ///Regular expression for parsing a feature key,value pair from a string
    let private featureRegexPattern = @"/(?<qualifierName>.+)="+  @"(?<qualifierValue>.+)"
    
    ///Regular expression for parsing a feature key,value pair that contains no value from a string
    let private featureRegexPattern2 = @"/(?<qualifierName>.+)"
    
    ///returns an key value pair for a feature from an input string. If the input string does not contain an "=" sign, the value belongs
    ///to the previous line and the function EXPLAIN THIS LATER
    let private parseFeatureQualifier (s: string) =
        if s.Contains "=" then
            match s with 
            |RegexGroups featureRegexPattern qual -> (qual.[0].["qualifierName"].Value),(qual.[0].["qualifierValue"].Value)
            |_ -> ".","."
        else
            match s with 
            |RegexGroups featureRegexPattern2 qual -> (qual.[0].["qualifierName"].Value),"."
            |_ -> ".","."
    

    ///functions for parsing a GenBank file.
    module Read =
    
        ///Token representing lines of a GenBank file for parsing purposes
        type private Token =
        ///Represents the lines ranking highest in hierarchy. These lines are not idented, and are parsed as key,value pair
        | Section of string*string
        ///Represents the lines ranking second in hierarchy. These lines are idented, but contain a key,value pair
        | Member of string*string
        ///Represents the lines ranking third in hierarchy. Features are only present in the features section of a GenBank file.
        ///These lines are idented and dont have a key.
        | Feature of string
        ///Represents the lines ranking lowest in hierarchy. These lines are idented and dont have a key. This union case indicates that the 
        ///value contained belongs to the next highest ranking line in hierarchy.
        | Value of string
        
    
        ///Splits the input string at a specific position (pos) returns two substrings of it, one sarting at (start) and
        ///ending at (pos), the other starting at (pos) and containing the rest of the string
        let private subStr start pos (str:string) =
            if start+pos < str.Length then 
                str.Substring(start,pos),(str.Substring(pos))
            else
                str,""
        ///Returns true if the input string is idented, otherwise returns false
        let private isIdent (s:string) = s.StartsWith " " 
        ///Retursn true if the input string is empty after being trimmed of whitespace, otherwise returns false
        let private isEmpty (s:string) =
            if s.Trim() = "" then true else false
        
        ///Returns a CurrentSection depending on an input key. Returns the input currentSection if the key does not indicate that the section changes.
        let private getCurrentSection (current: CurrentSection) (key : string) =
            match key.Trim() with
            |"REFERENCE"    -> CurrentSection.Reference
            |"FEATURES"     -> CurrentSection.Feature
            |"ORIGIN"       -> CurrentSection.Origin
            |_              -> current
        

        ///Assigns a string to its corresponding token type.
        let private lexer (sectionType:CurrentSection) (str:string) =
        
            let splitPos =
                match sectionType with
                |CurrentSection.Meta        ->  12
                |CurrentSection.Reference   ->  12
                |CurrentSection.Feature     ->  21
                |CurrentSection.Origin      ->  10
                |_ -> 0
        
            let (k,v) = subStr 0 splitPos str
            let cs = getCurrentSection sectionType k
            if isIdent k && not (isEmpty k) then
                cs,(Token.Member (k.Trim(),v.Trim()))
            elif isEmpty k then
                if cs = CurrentSection.Feature then
                    if v.Trim().StartsWith "/" then
                        cs,(Token.Feature (v.Trim()))
                    else
                        cs,(Token.Value (v.Trim()))
                else
                    cs,(Token.Value (v.Trim()))
            else
                cs,(Token.Section (k.Trim(),v.Trim()))
        
        ///Iterates over an input sequence of strings and returns a sequence containing the corresponding token for each entry.
        let private tokenizer (input:seq<string>) =
            let en = input.GetEnumerator()
            let rec loop (sectionType:CurrentSection) =
                seq {
                   match en.MoveNext() with
                   | false -> ()
                   | true -> 
                       let stype,token = lexer sectionType en.Current
                       yield (stype,token)
                       yield! loop stype
                  }
            loop CurrentSection.Meta


        ///Iterates over an input sequence of tokens and adds the corresponding GenBankItems to a dictionary. The returned dictionary represents a GenBank file.
        let private parser (originConverter:seq<char> -> 'a) (input:seq<CurrentSection*Token>) =
        
            let dict = new Dictionary<string,GenBankItem<'a>>()
            let en = input.GetEnumerator()
        
            let rec loop sec k v (ref:(string*string) list) (refList:(string*string)list list) featType featBs featQual (qname:string) (featQualList:FeatureQualifier list) (feat:Feature list) (origin:string) (isBs:bool) =
                if en.MoveNext() then
                    let nextSec, nextToken = en.Current
                    match sec,nextSec with
                    |CurrentSection.Meta,nextSec 
                        ->  match nextSec with
                            |CurrentSection.Meta
                                ->  match nextToken with
                                    |Member (k',v') when k' ="ORGANISM"
                                        ->  dict.Add(k,(GenBankItem<'a>.Value v))
                                            loop nextSec k' (v'.PadRight(67)) ref refList featType featBs featQual qname featQualList feat origin isBs
                                    |Section (k',v') |Member (k',v')
                                        ->  dict.Add(k,(GenBankItem<'a>.Value v))
                                            loop nextSec k' v' ref refList featType featBs featQual qname featQualList feat origin isBs
                                    |Token.Value v' 
                                        ->  loop nextSec k (v+v') ref refList featType featBs featQual qname featQualList feat origin isBs
                                    |_  ->  ()
                            |_  ->  dict.Add(k, GenBankItem<'a>.Value v)
                                    match nextToken with
                                    |Section (k',v')
                                        -> loop nextSec k' v' ref refList featType featBs featQual qname featQualList feat origin isBs
                                    |_  -> ()
                    
                    |CurrentSection.Reference,nextSec
                        ->  match nextSec with
                            |CurrentSection.Reference
                                ->  match nextToken with
                                    |Section (k',v') 
                                        ->  loop sec k' v' [] (((k,v)::ref)::refList) featType featBs featQual qname featQualList feat origin isBs
                                    |Member (k',v')
                                        ->  loop sec k' v' ((k,v)::ref) refList featType featBs featQual qname featQualList feat origin isBs
                                    |Token.Value v'
                                        -> loop nextSec k (v+v') ref refList featType featBs featQual qname featQualList feat origin isBs
                                    |_  -> ()
                            |_  ->  dict.Add("REFERENCES",References ((((k,v)::ref)::refList) |> List.rev |> List.map (fun x -> List.rev x)))
                                    match nextToken with
                                    |Section (k',v')
                                        ->  loop nextSec k' v' ref refList featType featBs featQual qname featQualList feat origin isBs
                                    |_  ->  ()
        
                    |CurrentSection.Feature,nextSec
                        ->  match nextSec with 
                            |CurrentSection.Feature when (k="FEATURES")
                                ->  match nextToken with 
                                    |Member (k',v')
                                        -> loop nextSec k' v' ref refList k' v' featQual qname featQualList feat origin true
                                    |_  -> ()
                            |CurrentSection.Feature 
                                ->  match nextToken with
                                    |Member (k',v')
                                        -> loop nextSec k' v' ref refList k' v' "" "" [] (((createFeature featType featBs (List.rev((createFeatureQualifier qname featQual)::(featQualList)))::feat))) origin true 
                                    |Feature f
                                        ->  let qualName,featureQualifier = parseFeatureQualifier f
                                            loop nextSec k v ref refList featType featBs featureQualifier qualName (if qname = "" then featQualList else ((createFeatureQualifier qname featQual )::featQualList) ) feat origin false
                                    |Token.Value v'
                                        ->  if isBs then
                                                loop nextSec k v' ref refList featType (featBs+v') featQual qname featQualList feat origin true
                                            else
                                                loop nextSec k v' ref refList featType featBs (featQual+v') qname featQualList feat origin false
                                    |_  -> ()
                            |_ ->   dict.Add("FEATURES",Features (List.rev (((createFeature featType featBs (List.rev((createFeatureQualifier qname featQual)::(featQualList)))::feat)))))
                                    match nextToken with
                                    |Section (k',v')
                                        ->  loop nextSec k' v' ref refList featType featBs featQual qname featQualList feat origin false
                                    |_  ->  ()
        
                    |CurrentSection.Origin,nextSec
                        ->  match nextSec with
                            |CurrentSection.Origin
                                -> match nextToken with
                                   |Member (k',v')
                                        -> loop nextSec k' v' ref refList featType featBs featQual qname featQualList feat (origin+v) false
                                   |_ -> dict.Add("ORIGIN",GenBankItem<'a>.Sequence((toCharArray (origin+v)) |> originConverter))
                            |_  -> dict.Add("ORIGIN",GenBankItem<'a>.Sequence((toCharArray (origin+v)) |> originConverter))
                    |_ -> ()
            if en.MoveNext() then
                match (snd en.Current) with
                |Section (k,v) ->
                    loop (fst en.Current) k v [] [] "." "." "" "" [] [] "" true
                |_ -> ()
                dict
            else 
                dict
        
        ///Returns a dictionary containing genBank items that represents the GenBank file at the input path
        ///taking a converter function for the origin sequence of the file 
        let fromFile (path:string) (originConverter:seq<char> -> 'a ) = 
            Seq.fromFile path
            |> tokenizer
            |> parser originConverter
        
        
        ///Returns a dictionary containing genBank items parsed from an input string sequence
        ///taking a converter function for the origin sequence 
        let fromSeq (originConverter:seq<char> -> 'a) (input:seq<string>) =
            input
            |> tokenizer
            |> parser originConverter
        
        module OriginConverters =
            open BioFSharp.BioSeq

            ///default converter that yields all characters of the origin sequence, skipping spaces.
            let defaultConverter (sequence : seq<char>) = seq{for i in sequence do if not (i=' ') then yield i}

            ///converts the origin sequence into a BioSeq of nucleotides
            let nucleotideConverter (sequence : seq<char>) = ofNucleotideString sequence

            ///converts the origin sequence into a BioSeq of amino acids
            let peptideConverter (sequence : seq<char>) = ofAminoAcidString sequence

    ///Functions for writing a GenBank file  
    module Write =
        
        ///constructs a sequence of strings in the right formatting (including identation of the key and the position for splitting key/value in the file) 
        ///from input key and value.
        let private constructSeqs (key:string) (value:string) split ident =

            let rec loop i splitSeq (words:string array) (line:string) lineList = 
                if i<words.Length then
                    if (line.Length + words.[i].Length) < 80 then
                        loop (i+1) splitSeq words (line+words.[i]) lineList
                    else
                        loop (i+1) splitSeq words (splitSeq+words.[i]) (line::lineList)
                else List.rev (line::lineList)
        
            let splitSeq = 
                [|for i=0 to split-1 do yield ' '|] |> fromCharArray
            
            let k' = key.PadLeft(key.Length+ident)
            let k = k'.PadRight(split)

            if value.Length < (80-split) then
                let line = k+value
                printfn "singleLine:%A" line
                seq{yield line}
            elif not (value.Contains " ") && not (value.Contains ",") then
                let x = 
                    value 
                    |> toCharArray
                    |> Seq.chunkBySize (79-split) 
                    |> Seq.mapi (fun i x -> if i=0 then k+(fromCharArray x) else splitSeq + (fromCharArray x))
                printfn "nonSeperated lines: %A" x
                x
            else 
                printfn "splitmagic..."
                let words = 
                    value 
                    |> toCharArray
                    |> Seq.groupAfter (fun x -> if x = ' ' || x = ',' then true else false)
                    |> Seq.toArray
                    |> Array.map (fun x -> List.toArray x |> fromCharArray)
                seq{ yield! loop 0 splitSeq words k []}
        

        ///creates a GenBank file at the specified path, taking a converter function for the origin sequence of the file 
        let write (path:string) (originConverter: 'a -> seq<char>) (gb : Dictionary<string,GenBankItem<'a>>)= 
            seq{
                for kv in gb do
                    let k,gbi = kv.Key,kv.Value
        
                    match gbi,k with
                        |GenBankItem.Value v,"ORGANISM" 
                            ->  yield! (constructSeqs k v 12 2) |> Seq.mapi (fun i x -> if i=0 then "  " + x.Trim() else x)
                        |GenBankItem.Value v,_          
                            ->  yield! (constructSeqs k v 12 0)
                        |References r,_       
                            ->  for i in r do
                                    for k',v' in i do
                                        match k' with
                                        |"REFERENCE"-> yield! constructSeqs k' v' 12 0
                                        |"PUBMED"   -> yield! constructSeqs k' v' 12 3
                                        |_          -> yield! constructSeqs k' v' 12 2
                        |Features f,_                   
                            ->  yield "FEATURES             Location/Qualifiers"
                                for feat in f do
                                    let k',bs = feat.Type,feat.BaseSpan
                                    yield! constructSeqs k' bs 21 5
                                    for qual in feat.Qualifiers do
                                        yield! constructSeqs "                    " ("/"+qual.Name+"="+qual.Value) 21 0
                        |GenBankItem.Sequence o,_
                            ->  yield "ORIGIN      "
                                let charSeq = 
                                    originConverter o
                                    |> Seq.chunkBySize 60
                                    |> Seq.map (fun x -> Seq.chunkBySize 10 x)
                                    |> Seq.map (fun x -> x |> Seq.foldi (fun i acc elem -> if not (i=5) then acc + (fromCharArray elem) + " " else  acc + (fromCharArray elem)) "")
                                    |> Seq.mapi (fun i x -> let k = string ((i*60) + 1)
                                                            let k' = k.PadLeft(9)
                                                            k' + " " + x)
                                yield! charSeq
                                yield "//"
                }
                |> FileIO.writeToFile false path
    
        
        module OriginConverters =
            open BioFSharp.BioSeq

            ///default converter that yields all characters of the origin sequence, skipping spaces.
            let defaultConverter (sequence : seq<char>) = id sequence

            ///converts the origin sequence into a BioSeq of nucleotides
            let nucleotideConverter (sequence : BioSeq<Nucleotides.Nucleotide>) = seq {yield! sequence |> BioSeq.toString}

            ///converts the origin sequence into a BioSeq of amino acids
            let peptideConverter (sequence : BioSeq<AminoAcids.AminoAcid>) = seq {yield! sequence |> BioSeq.toString}
        
    

    ///Returns all references of a GenBank file representation
    let getReferences (gb:Dictionary<string,GenBankItem<'a>>) =
        if gb.ContainsKey("REFERENCES") then
            match gb.["REFERENCES"] with 
            |References r
                ->  r
            |_  ->  printfn "unexpected type at key REFERENCES. Result is empty"
                    []
        else
            printfn "Collection does not contain references. Result is empty"
            []
    
    ///Returns all features of a GenBank file representation
    let getFeatures (gb:Dictionary<string,GenBankItem<'a>>) =
        if gb.ContainsKey("FEATURES") then
            match gb.["FEATURES"] with 
            |Features f
                ->    f
            |_  ->  printfn "unexpected type at key FEATURES. Result is empty"
                    []
        else
            printfn "Collection does not contain features. Result is empty"
            []
    
    ///Returns all features of a specific type of a GenBank file representation
    let getFeaturesWithType (featureType:string) (gb:Dictionary<string,GenBankItem<'a>>) =
        if gb.ContainsKey("FEATURES") then
            match gb.["FEATURES"] with
            |Features f ->  [for i in f do if i.Type = featureType then yield i]
            |_          ->  printfn "unexpected type at key FEATURES. Result is empty"
                            []
        else
            printfn "Collection does not contain features. Result is empty"
            []
    
    ///Returns the Origin of a GenBank file representation
    let getOrigin (gb:Dictionary<string,GenBankItem<'a>>) =
        if gb.ContainsKey("ORIGIN") then
            match gb.["ORIGIN"] with 
            |Sequence o   ->  o
            |_          ->  failwith "No Origin found"
        else
             failwith "No Origin found"
    
    ///Returns all Values of the meta section of a Genbank file representation
    let getValues (gb:Dictionary<string,GenBankItem<'a>>) = 
        [for i in gb do
            match i.Value with
            |GenBankItem.Value v    -> yield i.Key,v
            |_          -> ()
            ]
    
    ///Returns a GenBank item at the specified key, if it exists in the dictionary
    let tryGetItem (key:string) (gb:Dictionary<string,GenBankItem<'a>>) =
        if gb.ContainsKey key then
            Some gb.[key]
        else
            None