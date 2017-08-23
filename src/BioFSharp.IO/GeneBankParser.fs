namespace BioFSharp.IO

///A collection of functions for reading and writing GeneBank files.
module GenBankParser =
        
    open BioFSharp
    open System.IO
    open System
    open FSharp.Care.IO
    open FSharp.Care.Collections.Seq
    open FSharp.Care
    open FSharp.Care.Regex
    
//=================================================================== Types ==========================================================================
    
    ///Represents the possible section locations in a GeneBank file
    type CurrentSection = 
        Meta = 1 |Reference = 2| Feature = 3 |Origin = 4 |End = 5


    ///Represents the upper part of the header of a GeneBank file.
    type Meta = {
        Locus       :   string
        Definition  :   string
        Accession   :   string
        Version     :   string
        Keywords    :   string
        Source      :   string
        Organism    :   string
    }
    
    let createMeta locus def acc ver kw src org = {Locus=locus; Definition=def; Accession=acc; Version=ver; Keywords=kw; Source=src; Organism=org}

    let metaDefault = {
        Locus       =   "."
        Definition  =   "."
        Accession   =   "."
        Version     =   "."
        Keywords    =   "."
        Source      =   "."
        Organism    =   "."
    }
        

    ///Represents the lower part of the header of a GeneBank file.
    type Reference = {
        Ref     :   string
        Authors :   string
        Title   :   string
        Journal :   string
        Pubmed  :   string
    }
    
    let createReference ref authors title journal pubmed = {Ref=ref; Authors=authors; Title=title; Journal=journal; Pubmed=pubmed} 
    
    let referenceDefault = {
        Ref     =   "."
        Authors =   "."
        Title   =   "."
        Journal =   "."
        Pubmed  =   "."
    }
    
    
    ///Represents the base span of a feature
    //lots of refactoring needed here
    type FeatureBaseSpan =
        |CompleteSpan       of (int*int)
        |FivePrimePartial   of (int*int)
        |ThreePrimePartial  of (int*int)
        |Complement         of (int*int)
        |Default            of (int*int)
    
    
    let parseBaseSpan (s:string) =
        if s.Contains "complement" then
            printfn "%i" 1
            match s with
            | RegexGroups "(?<begin>\d+)([.<>])*((?<end>\d+))" bs -> Complement (int bs.[0].["begin"].Value,int bs.[0].["end"].Value)
            | _ -> Default (-1,-1)
        elif s.StartsWith "<" then
            printfn "%i" 2
            match s with
            | RegexGroups "(?<begin>\d+)([.<>])*((?<end>\d+))" bs  -> FivePrimePartial (int bs.[0].["begin"].Value,int bs.[0].["end"].Value)
            | _ -> Default (-1,-1)
        elif s.Contains ">" then
            printfn "%i" 3
            match s with
            | RegexGroups "(?<begin>\d+)([.<>])*((?<end>\d+))" bs  -> ThreePrimePartial (int bs.[0].["begin"].Value,int bs.[0].["end"].Value)
            | _ -> Default (-1,-1)
        else
            printfn "%i" 4
            match s with
            | RegexGroups "(?<begin>\d+)([.<>])*((?<end>\d+))" bs  -> CompleteSpan (int bs.[0].["begin"].Value,int bs.[0].["end"].Value)
            | _ -> Default (-1,-1)
     
    
    type FeatureQualifier = {
        Name:string;
        Value:string
        }
    
    let defaultFeatureQualifier = {
        Name = ".";
        Value = "."
        }
    
    let createFeatureQualifier name value = {Name=name;Value=value} 
    
    let parseFeatureQualifier (s: string) =
        match s with 
        |RegexGroups @"(?<qualifierName>.+)=(?<qualifierValue>.+)" qual -> createFeatureQualifier qual.[0].["qualifierName"].Value qual.[0].["qualifierValue"].Value
        |_ -> defaultFeatureQualifier
    
    type Feature = {
        Type        : string;
        BaseSpan    : FeatureBaseSpan;
        Qualifiers  : FeatureQualifier list
    }
    
    let createFeature t bs qual = {Type = t; BaseSpan=bs; Qualifiers=qual}
    
    type Features = Feature list
    
    let featureDefault = {
        Type = ".";
        BaseSpan = CompleteSpan (-1,-1);
        Qualifiers = [defaultFeatureQualifier]
    }
    
    let featuresDefault : Features = [featureDefault]

    ///Represents the origin part of a GeneBank file.
    type Origin = Map<int,string>
    
    let originDefault = new Origin([(-1,".")])


    ///Represents a GeneBank file consistign of Meta, Reference, Features and Origin section.
    type GeneBank = {
        Meta : Meta
        References : Reference list
        Features: Features
        Origin: Origin
    }

//=================================================================== Helper Functions ==========================================================================
    
    ///returns the input as a sequence of strings, one line at a time
    let readFile filePath  = FileIO.readFile(filePath)
    
    ///returns a trimmed tuple created from an input string, splitted at the input position
    let splitToKV (s:string) (pos:int) =
        let key = s.[0..pos-1].Trim()
        let value = s.[pos..].Trim()
        (key,value)
    
    ///returns the value belonging to a query key, or a default value if the key is not present in the map.
    let valueOrDefault (map : Map<'key,'value>) (key: 'key) (defaultValue : 'value) =    
        match Map.tryFind key map with
        |Some s -> s
        |None -> defaultValue

        
    let setNextSectionFromKey (key: string) =
        match key with 
        |"REFERENCE"    -> CurrentSection.Reference
        |"FEATURES"     -> CurrentSection.Feature
        |"ORIGIN"       -> CurrentSection.Origin
        |_              -> CurrentSection.End
    

    //let parseFrom (lineIndex: int) (inputFile:string) (parser: Collections.Generic.IEnumerator<string> -> string -> string -> 'a) =
    //    let temp = readFile inputFile
    //    let section = seq{

    


//===================================================================== META PARSER =============================================================================
    // Input: 
    //  - enum:     an enumerator of the input sequence.
    //  - initKey:  the key at the current position of the enumerator
    //  - initValue the value at the current position of the enumerator

    //!Make sure to move the enumerator at the position where the Meta section starts in your input file if you use this function standalone!

    ///Parses the upper section of the header until the 'Reference' keyword is found. Returns the Meta type filled with the parsed key value pairs or default values
    let metaParser (enum : Collections.Generic.IEnumerator<string>) (initKey:string) (initValue:string) =
        let rec loop key value l = 
    
            //keep the enumeration going until the 'REFERENCE' keyword is found.
            if key <> "REFERENCE" then
                if enum.MoveNext() then
    
                    //split the line into a key value pair
                    let (k,v) = splitToKV enum.Current 12
    
                    //if the key is not empty, the line belongs to the previous key
                    if k = "" then 
                        loop key (value + " " + v) l
    
                    //otherwise, a new key is found
                    else 
                        loop k v ((key,value)::l)
    
                //The file is ccorrupted, if there are no following sections.
                //maybe catch this elsewhere?
                else failwith "wrong input format"
    
            //Meta section ends
            else
                //create a Map of the key value pairs and construct the Meta type from it. Default values are inserted as values for missing fields.
                let temp = (List.rev l) |> Map.ofList
                {
                    Locus       =   valueOrDefault temp "LOCUS" "."
                    Definition  =   valueOrDefault temp "DEFINITION" "."
                    Accession   =   valueOrDefault temp "ACCESSION" "."
                    Version     =   valueOrDefault temp "VERSION" "."
                    Keywords    =   valueOrDefault temp "KEYWORDS" "."
                    Source      =   valueOrDefault temp "SOURCE" "."
                    Organism    =   valueOrDefault temp "ORGANISM" "."
                }
        loop initKey initValue []
    

    
    //============================================================ Reference parser ==========================================================
    // Input: 
    //  - enum:     an enumerator of the input sequence.
    //  - initKey:  the key at the current position of the enumerator
    //  - initValue the value at the current position of the enumerator

    //!Make sure to move the enumerator at the position where the Reference section starts in your input file if you use this function standalone!

    ///Parses the lower section of the header until the 'Features' keyword is found. Returns a List of Reference types filled with the parsed key value pairs or default values
    let referenceParser (enum: Collections.Generic.IEnumerator<string>) initK initV =
    
        //use this mutable to ensure that the first Reference keyword does not trigger a new reference group
        let mutable isFirstGroup = true
        let rec loop key value l outerL =
    
            //keep the enumeration going until the 'FEATURES' keyword is found.
            if key <> "FEATURES" then
    
                if enum.MoveNext() then
                    
                    //collect the key value pairs belonging to one Reference group
                    if key <> "REFERENCE" || isFirstGroup then
    
                        isFirstGroup <- false
                        let (k,v) = splitToKV enum.Current 12
    
                        //when the key is empty, the value belongs to the previous line
                        if k = "" then
                            loop key (value+ " "+ v) l outerL
                        else 
                            loop k v ((key,value)::l) outerL
    
                    else 
                        //start a new group for the next Reference
                        let (k,v) = splitToKV enum.Current 12
                        if k = "" then
                            loop key (value+ " "+ v) [] (l::outerL)
                        else 
                            loop k v [(key,value)] (l::outerL)
    
                else failwith "wrong input format"
        
            else 
                let temp = l::outerL |> List.rev |> List.map (fun x -> Map.ofList x)
                temp |> List.map (fun x -> {
                                                Ref     =   valueOrDefault x "REFERENCE" "."
                                                Authors =   valueOrDefault x "AUTHORS" "."
                                                Title   =   valueOrDefault x "TITLE" "."
                                                Journal =   valueOrDefault x "JOURNAL" "."
                                                Pubmed  =   valueOrDefault x "PUBMED" "."
                                            })
                
        loop initK initV [] []
        
    
    //============================================================= Features Parser ============================================================
    // Input: 
    //  - enum:     an enumerator of the input sequence. 
    //  - initKey:  the key at the current position of the enumerator
    //  - initValue the value at the current position of the enumerator

    //!Make sure to move the enumerator at the position where the Features section starts in your input file if you use this function standalone!

    ///Parses the Features section of a GeneBank file until the 'Origin' keyword is found. Returns a List keys representing 
    ///the feature name, toupled with a list of the features belonging to that key
    let featuresParser (enum: Collections.Generic.IEnumerator<string>) initK initV : Features =
    
        //
        //let mutable isFirstGroup = true
        let rec loop (key,value) valueList outerL =
            
            //keep the enumeration going until the 'FEATURES' keyword is found.
            if key <> "ORIGIN" then
    
                if enum.MoveNext() then
                    
    
                    //collect the key value pairs belonging to one Reference group
                    if enum.Current.Length > 21 then
    
                        let (k,v) = splitToKV enum.Current 21
    
                        printfn "%A" (key,v)
    
                        //when the key is empty, the value belongs to the previous line
                        if k = "" then
                            loop (key,value) (v::valueList) outerL
                        else 
                            loop (k,v) [] (((key,value),valueList)::outerL)
    
                    elif enum.Current.Contains "ORIGIN" then
                        let temp = ((key,value),valueList)::outerL |> List.rev |> List.map (fun (x,y) -> (x,List.rev y))
                        temp
                        |> List.map (fun ((n,bs),y) -> createFeature n (parseBaseSpan bs) (y |> List.map (fun x -> parseFeatureQualifier x)))
                        |> List.tail
                    else 
                        failwith "wrong imput format in features section"
    
                else failwith "wrong input format in features section"
        
            else 
                let temp = ((key,value),valueList)::outerL |> List.rev 
                temp
                |> List.map (fun ((n,bs),y) -> createFeature n (parseBaseSpan bs) (y |> List.map (fun x -> parseFeatureQualifier x)))
                |> List.tail
        loop (initK,initV) [] []
        
    
    
    //============================================================= Origin Parser ============================================================
    // Input: 
    //  - enum:     an enumerator of the input sequence. 
    //  - initKey:  the key at the current position of the enumerator
    //  - initValue the value at the current position of the enumerator

    //!Make sure to move the enumerator at the position where the Origin section starts in your input file if you use this function standalone!

    ///Parses the Origin section of a GeneBank file. Returns a Map mappping the position in the sequence to the nucleotide sequences starting from that position
    let originParser (enum : Collections.Generic.IEnumerator<string>) (initKey:string) (initValue:string) =
        let rec loop key value l = 
            if enum.MoveNext() then
                if enum.Current <> "//" then
                    let (k,v) = splitToKV enum.Current 10
                    if k = "" then 
                        loop key (value + " " + v) l
                    else 
                        loop k v ((key,value)::l)
                else
                    new Origin((List.rev l).Tail |> List.map (fun (x,y) -> (int x, y)))
            else failwith "wrong input format"
    
        loop initKey initValue []
    
    
    
    //============================================================= GeneBank Parser ===========================================================
   
    let readGBFile (path:string) =

        let input = readFile path
        use en = input.GetEnumerator()

        let rec loop section meta reference feature origin =
            match section with 
            |CurrentSection.Meta ->
                //String split is at 12
                //MetaParser until Source keyword
                let key' = en.Current.[0..11].Trim()
                let value = en.Current.[12..].Trim()
    
                loop CurrentSection.Reference (metaParser en key' value) reference feature origin 
    
            |CurrentSection.Reference ->
                //String split is at 12
                //ReferenceParser Bis Features
                let key' = en.Current.[0..11].Trim()
                let value = en.Current.[12..].Trim()
    
                loop CurrentSection.Feature meta (referenceParser en key' value) feature origin
    
            |CurrentSection.Feature ->
                //String split is at 21
                let key' = en.Current.[0..20].Trim()
                let value = en.Current.[21..].Trim()
    
                loop CurrentSection.Origin meta reference (featuresParser en key' value) origin
    
            |CurrentSection.Origin ->
                //String split is at 10
                let key' = en.Current.[0..9].Trim()
                let value = en.Current.[10..].Trim()
    
                loop CurrentSection.End meta reference feature (originParser en key' value)
            
            |CurrentSection.End ->

                {
                    Meta = meta;
                    References = reference
                    Features = feature
                    Origin = origin
                }
            
            | _ -> failwith "unexpected enum type"
    
        if en.MoveNext() then loop CurrentSection.Meta metaDefault [referenceDefault] featuresDefault originDefault else failwith "emptyInput"

    