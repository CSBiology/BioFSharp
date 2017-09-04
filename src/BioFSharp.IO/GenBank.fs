namespace BioFsharp.IO

///functions for reading and writing GenBank(.gb) files
module GenBank =

    open System
    open FSharp.Care
    open FSharp.Care.IO
    open FSharp.Care.Regex
    open System.Collections.Generic
    open System.Text
    
    ///Iterate over an array yielding the result of applying a function to each element in a sequence
    let rec private arrayIteriYield (mapping: int -> 'a -> 'b) (a:array<'a>) =
        seq{
            for i=0 to a.Length-1 do
                yield mapping i a.[i]
            }

    ///Represents a single reference from the REFERENCE section of a GenBank file. A Reference represents publications by the authors of the sequence that discuss the data reported in the record.
    type Reference = 
        {
        ///String containing the number of the reference and its base span.
        Ref                     :   string
        ///List of authors in the order in which they appear in the cited article.
        Authors                 :   string
        ///Title of the published work or tentative title of an unpublished work. 
        Title                   :   string
        ///MEDLINE abbreviation of the journal name
        Journal                 :   string
        ///PubMed Identifier (PMID)
        Pubmed                  :   string
        ///Free text comment about this reference
        Remark                  :   string
        ///Any other fields contained in a reference are parsed into this key,value list
        AdditionalInformation   :   (string*string) list
        }
        ///returns a sequence of strings for writing this type to a genBank file with correct formatting
        member this.toFileString =
            seq{
                yield ("REFERENCE   " + this.Ref)
                if not (this.Authors = ".") then yield! ((this.Authors.Split '\n')  |> arrayIteriYield (fun i x -> if i=0 then "  AUTHORS   "+x else "            "+x))
                if not (this.Title   = ".") then yield! ((this.Title.Split '\n')    |> arrayIteriYield (fun i x -> if i=0 then "  TITLE     "+x else "            "+x))
                if not (this.Journal = ".") then yield! ((this.Journal.Split '\n')  |> arrayIteriYield (fun i x -> if i=0 then "  JOURNAL   "+x else "            "+x))
                if not (this.Pubmed  = ".") then yield! ((this.Pubmed.Split '\n')   |> arrayIteriYield (fun i x -> if i=0 then "   PUBMED   "+x else "            "+x))
                if not (this.Remark  = ".") then yield! ((this.Remark.Split '\n')   |> arrayIteriYield (fun i x -> if i=0 then "  REMARK    "+x else "            "+x))
                for (k,v) in this.AdditionalInformation do 
                    let sb = new StringBuilder()
                    yield! 
                        ((v.Split '\n')   
                        |> arrayIteriYield (fun i x -> 
                            for a=0 to 12-k.Length+1 do sb.Insert(0," ") |> ignore
                            if i=0 then "  " + k + sb.ToString() + x else "            "+x))
                }

    let createReference ref authors title journal pubmed rem ai = {Ref=ref; Authors=authors; Title=title; Journal=journal; Pubmed=pubmed; Remark=rem; AdditionalInformation=ai} 

    ///Represents a single feature Qualifier and its value from the FEATURES section of a Genbank file. Features can contain
    ///Information about genes and gene products, as well as regions of biological significance reported in the sequence
    type FeatureQualifier = {
        ///Key of the Feature
        Name:string;
        ///Value of the Feature
        Value:string
        }
    
    ///create a FeatureQualifier from input c
    let createFeatureQualifier name value = {Name=name;Value=value} 

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


    ///Represents the origin part of a GeneBank file.
    type Origin = Map<int,string>

    ///Represents any Item a GenBank file can contain as a union case. The result of parsing a genBank file will be a dictionary containing this type.
    type GenBankItem =
        ///Any value contained in the meta section of a GenBank file. 
        |Value of string
        ///All references contained in a GenBank file is seperate entries in a list.
        |References of Reference list
        ///All features contained in a GenBank file as seperate entries in a list
        |Features of Feature list
        ///The origin section of a GenBank file as a map mapping indices(int) to a sequence (string)
        |Origin of Origin
    
    ///Functions for reading a GenBank file
    module Read =
            
        ///Represents the possible sections in a GenBank file
        type private CurrentSection = 
            Meta = 1 |Reference = 2| Feature = 3 |Origin = 4 |End = 5

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
        let private parser (input:seq<CurrentSection*Token>) =
        
            let dict = new Dictionary<string,GenBankItem>()
        
            let en = input.GetEnumerator()
        
            let rec loop sec token k v refName refAut refTtl refJnl refPmed (refRem:string) (refAi:(string*string) list) (ref:Reference list) featType featBs featQual (qname:string) (featQualList:FeatureQualifier list) (feat:Feature list) (origin:Origin) (isBs:bool) =
                match sec with
                |CurrentSection.Meta
                    ->  if en.MoveNext() then
                            let nextSec, nextToken = en.Current
                            match nextSec with
                            |CurrentSection.Meta
                                ->  match nextToken with
                                    |Section (k',v')
                                        ->  dict.Add(k,(GenBankItem.Value v))
                                            loop nextSec nextToken k' v' refName refAut refTtl refJnl refPmed refRem refAi ref featType featBs featQual qname featQualList feat origin isBs
                                    |Member (k',v')
                                        ->  dict.Add(k,(GenBankItem.Value v))
                                            loop nextSec nextToken k' v' refName refAut refTtl refJnl refPmed refRem refAi ref featType featBs featQual qname featQualList feat origin isBs
                                    |Token.Value v' 
                                        ->  loop nextSec nextToken k (v+"\n"+v') refName refAut refTtl refJnl refPmed refRem refAi ref featType featBs featQual qname featQualList feat origin isBs
                                    |_  ->  printfn "parsing stopped in Meta section. please verify file format. This could be a result of false identation in this section. The result possibly does not contain the whole file."
                            |_ ->   dict.Add(k, GenBankItem.Value v)
                                    match nextToken with
                                    |Section (k',v')
                                        -> loop nextSec nextToken k' v' refName refAut refTtl refJnl refPmed refRem refAi ref featType featBs featQual qname featQualList feat origin isBs
                                    |_  -> printfn "parsing stopped after finishing the Meta section. please verify file format. This could be a result of false identation in the Reference section. The result possibly does not contain the whole file."
                
                |CurrentSection.Reference
                    ->  if en.MoveNext() then
                            let nextSec, nextToken = en.Current
                            match nextSec with
                            |CurrentSection.Reference
                                ->  match nextToken with
                                    |Section (k',v') 
                                        ->  match k with
                                            |"REFERENCE"    ->  loop nextSec nextToken k' v' "." "." "." "." "." "." [] ((createReference v refAut refTtl refJnl refPmed refRem refAi)::ref) featType featBs featQual qname featQualList feat origin isBs
                                            |"AUTHORS"      ->  loop nextSec nextToken k' v' "." "." "." "." "." "." [] ((createReference refName v refTtl refJnl refPmed refRem refAi)::ref) featType featBs featQual qname featQualList feat origin isBs
                                            |"TITLE"        ->  loop nextSec nextToken k' v' "." "." "." "." "." "." [] ((createReference refName refAut v refJnl refPmed refRem refAi)::ref) featType featBs featQual qname featQualList feat origin isBs
                                            |"JOURNAL"      ->  loop nextSec nextToken k' v' "." "." "." "." "." "." [] ((createReference refName refAut refTtl v refPmed refRem refAi)::ref) featType featBs featQual qname featQualList feat origin isBs
                                            |"PUBMED"       ->  loop nextSec nextToken k' v' "." "." "." "." "." "." [] ((createReference refName refAut refTtl refJnl v refRem refAi)::ref) featType featBs featQual qname featQualList feat origin isBs
                                            |"REMARK"       ->  loop nextSec nextToken k' v' "." "." "." "." "." "." [] ((createReference refName refAut refTtl refJnl refPmed v refAi)::ref) featType featBs featQual qname featQualList feat origin isBs
                                            |_              ->  loop nextSec nextToken k' v' "." "." "." "." "." "." [] ((createReference refName refAut refTtl refJnl refPmed refRem ((k,v)::refAi))::ref) featType featBs featQual qname featQualList feat origin isBs
                                    |Member (k',v')
                                        ->  match k with 
                                            |"REFERENCE"    ->  loop nextSec nextToken k' v' v refAut refTtl refJnl refPmed refRem refAi ref featType featBs featQual qname featQualList feat origin isBs
                                            |"AUTHORS"      ->  loop nextSec nextToken k' v' refName v refTtl refJnl refPmed refRem refAi ref featType featBs featQual qname featQualList feat origin isBs
                                            |"TITLE"        ->  loop nextSec nextToken k' v' refName refAut v refJnl refPmed refRem refAi ref featType featBs featQual qname featQualList feat origin isBs
                                            |"JOURNAL"      ->  loop nextSec nextToken k' v' refName refAut refTtl v refPmed refRem refAi ref featType featBs featQual qname featQualList feat origin isBs
                                            |"PUBMED"       ->  loop nextSec nextToken k' v' refName refAut refTtl refJnl v refRem refAi ref featType featBs featQual qname featQualList feat origin isBs
                                            |"REMARK"       ->  loop nextSec nextToken k' v' refName refAut refTtl refJnl refPmed v refAi ref featType featBs featQual qname featQualList feat origin isBs
                                            |_              ->  loop nextSec nextToken k' v' refName refAut refTtl refJnl refPmed refRem ((k,v)::refAi) ref featType featBs featQual qname featQualList feat origin isBs
                                    |Token.Value v'
                                        -> loop nextSec nextToken k (v+"\n"+v') refName refAut refTtl refJnl refPmed refRem refAi ref featType featBs featQual qname featQualList feat origin isBs
                                    |_  -> printfn "parsing stopped in Reference section. please verify file format. This could be a result of false identation in this section. The result possibly does not contain the whole file."
                            |_ ->   
                                match k with 
                                |"REFERENCE"    ->  dict.Add("REFERENCES",(References(((createReference v refAut refTtl refJnl refPmed refRem refAi)::ref) |> List.rev)))
                                |"AUTHORS"      ->  dict.Add("REFERENCES",(References(((createReference refName v refTtl refJnl refPmed refRem refAi)::ref) |> List.rev)))
                                |"TITLE"        ->  dict.Add("REFERENCES",(References(((createReference refName refAut v refJnl refPmed refRem refAi)::ref) |> List.rev)))
                                |"JOURNAL"      ->  dict.Add("REFERENCES",(References(((createReference refName refAut refTtl v refPmed refRem refAi)::ref) |> List.rev)))
                                |"PUBMED"       ->  dict.Add("REFERENCES",(References(((createReference refName refAut refTtl refJnl v refRem refAi)::ref) |> List.rev)))
                                |"REMARK"       ->  dict.Add("REFERENCES",(References(((createReference refName refAut refTtl refJnl refPmed v refAi)::ref) |> List.rev)))
                                |_              ->  dict.Add("REFERENCES",(References(((createReference refName refAut refTtl refJnl refPmed refRem ((k,v)::refAi))::ref) |> List.rev)))
                                match nextToken with
                                |Section (k',v')
                                   ->  loop nextSec nextToken k' v' refName refAut refTtl refJnl refPmed refRem refAi ref featType featBs featQual qname featQualList feat origin isBs
                                |_  ->  printfn "parsing stopped after finishing the Reference section. please verify file format. This could be a result of false identation in the Features section. The result possibly does not contain the whole file."
        
                |CurrentSection.Feature
                    ->  if en.MoveNext() then
                            let nextSec, nextToken = en.Current
                            match nextSec with 
                            |CurrentSection.Feature when (k="FEATURES")
                                ->  match nextToken with 
                                    |Member (k',v')
                                        -> loop nextSec nextToken k' v' refName refAut refTtl refJnl refPmed refRem refAi ref k' v' featQual qname featQualList feat origin true
                                    |_  -> printfn "parsing stopped at the start of the Features section. please verify file format. This could be a result of false identation in this section. The result possibly does not contain the whole file."
                            |CurrentSection.Feature 
                                ->  match nextToken with
                                    |Member (k',v')
                                        -> loop nextSec nextToken k' v' refName refAut refTtl refJnl refPmed refRem refAi ref k' v' "" "" [] (((createFeature featType featBs (List.rev((createFeatureQualifier qname featQual)::(featQualList)))::feat))) origin true 
                                    |Feature f
                                        ->  let qualName,featureQualifier = parseFeatureQualifier f
                                            loop nextSec nextToken k v refName refAut refTtl refJnl refPmed refRem refAi ref featType featBs featureQualifier qualName (if qname = "" then featQualList else ((createFeatureQualifier qname featQual )::featQualList) ) feat origin false
                                    |Token.Value v'
                                        ->  if isBs then
                                                loop nextSec nextToken k v' refName refAut refTtl refJnl refPmed refRem refAi ref featType (featBs+"\n"+v') featQual qname featQualList feat origin true
                                            else
                                                loop nextSec nextToken k v' refName refAut refTtl refJnl refPmed refRem refAi ref featType featBs (featQual+"\n"+v') qname featQualList feat origin false
                                    |_  -> printfn "parsing stopped in Features section. please verify file format. This could be a result of false identation in this section. The result possibly does not contain the whole file."
                            |_ ->   dict.Add("FEATURES",Features (List.rev (((createFeature featType featBs (List.rev((createFeatureQualifier qname featQual)::(featQualList)))::feat)))))
                                    match nextToken with
                                    |Section (k',v')
                                        ->  loop nextSec nextToken k' v' refName refAut refTtl refJnl refPmed refRem refAi ref featType featBs featQual qname featQualList feat origin false
                                    |_  ->  printfn "parsing stopped after finishing the Features section. If your file contains an origin section, this could be a result of false identation. The result possibly does not contain the whole file."
        
                |CurrentSection.Origin  
                    ->  if en.MoveNext() then
                            let nextSec, nextToken = en.Current 
                            match nextSec with
                            |CurrentSection.Origin when k = "ORIGIN"
                                ->  match nextToken with
                                    |Member (k',v')
                                        -> loop nextSec nextToken k' v' refName refAut refTtl refJnl refPmed refRem refAi ref featType featBs featQual qname featQualList feat origin isBs
                                    |_  -> printfn "parser found no origin section. if this is unexpected, please verify the file format."
                            |CurrentSection.Origin
                                ->  match nextToken with
                                    |Member (k',v')
                                        ->  if not (origin.ContainsKey (int k))
                                                then loop nextSec nextToken k' v' refName refAut refTtl refJnl refPmed refRem refAi ref featType featBs featQual qname featQualList feat (Map.add (int k) v origin) isBs
                                            else
                                                loop nextSec nextToken k' v' refName refAut refTtl refJnl refPmed refRem refAi ref featType featBs featQual qname featQualList feat origin isBs
                                    |Section (k',v') 
                                        ->  if k' = "//" then
                                                dict.Add ("ORIGIN",GenBankItem.Origin (Map.add (int k) v origin)) 
                                            else
                                                printfn "parsing stopped in the Origin section. please verify file format. This could be a result of false identation in this section. The result possibly does not contain the whole file."
                                    |_  -> printfn "parsing stopped in the Origin section. please verify file format. This could be a result of false identation in this section. The result possibly does not contain the whole file."
                            |_  -> printfn "parsing stopped in the Origin section. please verify file format. This could be a result of false identation in this section. The result possibly does not contain the whole file."
                |_  -> dict.Add ("ORIGIN",GenBankItem.Origin ((Map.add (int k) v origin)))
        
            if en.MoveNext() then
                match (snd en.Current) with
                |Section (k,v) ->
                    loop (fst en.Current) (snd en.Current) k v "." "." "." "." "." "." [] [] "." "." "" "" [] [] (Map.empty<int,string>) true
                
                |_ -> printfn "parsing failed. This could be a result of false identation in the first line. please verify the file format. The result will not contain any entries."
                dict
            else 
                printfn "The input file was empty. The result will not contain any entries."
                dict
        
        ///Returns a dictionary containing genBank items that represents the GenBank file at the input path
        let fromFile (path:string) = 
            if path.EndsWith ".gb" then
                Seq.fromFile path
                |> tokenizer
                |> parser
            else failwith "incorrect file format"

        ///Returns a dictionary containing genBank items parsed from an input string sequence
        let fromSeq (input:seq<string>) =
            input
            |> tokenizer
            |> parser

    ///Functions for writing a GenBank file
    module Write =
        open FSharp.Care.String

        ///returns the position that a string should be splitted to a key,value pair depending on the type of the input GenBankItem
        let private getSplitFromGeneBankItem (gbi:GenBankItem) =
            match gbi with
                |Value v                ->  12
                |References r           ->  12
                |Features f             ->  21
                |GenBankItem.Origin o   ->  10

        ///Returns the key correctly idented for a value in the meta section
        let private getIdentation (key:string) (gbi:GenBankItem) =
            let x = new StringBuilder(key)
            let y = new StringBuilder()
            match gbi with
            |Value v -> 
                match key with
                |"ORGANISM" ->  let before, after = 2,2
                                for i in 0..(before-1) do x.Insert(0," ") |> ignore
                                for i in 0..(after-1) do y.Insert(0," ") |> ignore
                                x.ToString()+y.ToString()
                |s          ->  let before, after = 0,(12-s.Length) 
                                for i in 0..(before-1) do x.Insert(0," ") |> ignore
                                for i in 0..(after-1) do y.Insert(0," ") |> ignore
                                x.ToString()+y.ToString()
            |_ ->   (0,0) |> ignore 
                    ""

        ///returns a sequence of strings representing the input GenBankItem in correct formatting 
        let private stringyfy (key:string) (gbi:GenBankItem) = 

            let split = getSplitFromGeneBankItem gbi
            let whiteSpace = [|for i in 0 .. split-1 do yield ' '|] |> fromCharArray
            seq {
                match gbi with
                |Value v -> 
                    printfn "val"
                    let ident = getIdentation key gbi
                    let splittedVal = v.Split '\n'
                    yield! (splittedVal |> arrayIteriYield (fun i x -> if i=0 then ident+x else whiteSpace+x))

                |References r ->
                    printfn "ref"
                    for i in r do yield! i.toFileString
            
            
                |Features f -> 
                        yield "FEATURES             Location/Qualifiers" 
                        for feat in f do

                            let tmp = "     " + feat.Type
                            let featType = tmp.PadRight(21) 
                            yield! ((feat.BaseSpan.Split '\n') |> arrayIteriYield (fun i x -> if i=0 then featType+x else "                     "+x))

                            for fq in feat.Qualifiers do
                                yield! 
                                    ((fq.Value.Split '\n') 
                                    |> arrayIteriYield 
                                        (fun i x 
                                            ->  if i=0 then
                                                    if not (x = ".")
                                                        then "                     /"+fq.Name+"="+x 
                                                    else 
                                                        "                     /"+fq.Name
                                                else 
                                                    
                                                    "                     "+x))
                |Origin o              
                        ->  yield "ORIGIN      "
                            for kvp in o do
                                let key = (string kvp.Key + " ").PadLeft(10)
                                yield key+kvp.Value
                            yield "//"
                            yield ""
                        }

        let toStringSeq (gb:Dictionary<string,GenBankItem>) =
            seq {
                for i in gb do 
                    let lines = stringyfy i.Key i.Value
                    printfn "%A" lines
                    yield! lines

                if gb.ContainsKey "ORIGIN" then
                    ()
                else
                    yield "ORIGIN      "
                    yield "//"
                    yield ""
              }

        ///Creates a GenBank file representing the input dictionary of GenBank items at the specified path 
        let toFile (path:string) (gb:Dictionary<string,GenBankItem>) =
            toStringSeq gb |> FileIO.writeToFile false path

        let toString (gb:Dictionary<string,GenBankItem>) =
            toStringSeq gb 
            |> Seq.fold (fun acc elem -> acc + elem + "\n") ""

    ///Returns all references of a GenBank file representation
    let getReferences (gb:Dictionary<string,GenBankItem>) =
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
    let getFeatures (gb:Dictionary<string,GenBankItem>) =
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
    let getFeaturesWithType (featureType:string) (gb:Dictionary<string,GenBankItem>) =
        if gb.ContainsKey("FEATURES") then
            match gb.["FEATURES"] with
            |Features f ->  [for i in f do if i.Type = featureType then yield i]
            |_          ->  printfn "unexpected type at key FEATURES. Result is empty"
                            []
        else
            printfn "Collection does not contain features. Result is empty"
            []
    
    ///Returns the Origin of a GenBank file representation
    let getOrigin (gb:Dictionary<string,GenBankItem>) =
        if gb.ContainsKey("ORIGIN") then
            match gb.["ORIGIN"] with 
            |Origin o   ->  o
            |_          ->  printfn "unexpected type at key ORIGIN. Result is empty"
                            Map.empty<int,string>
        else
            printfn "Collection does not contain an origin. Result is empty"
            Map.empty<int,string>
    
    ///Returns all Values of the meta section of a Genbank file representation
    let getValues (gb:Dictionary<string,GenBankItem>) = 
        [for i in gb do
            match i.Value with
            |Value v    -> yield i.Key,v
            |_          -> ()
            ]
    
    ///Returns a GenBank item at the specified key, if it exists in the dictionary
    let tryGetItem (key:string) (gb:Dictionary<string,GenBankItem>) =
        if gb.ContainsKey key then
            Some gb.[key]
        else
            None