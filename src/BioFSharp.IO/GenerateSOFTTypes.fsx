#I @"../../bin/BioFSharp.IO/net47/"
#r "FSharpAux.IO.dll"
#r "FSharpAux.dll"

open FSharpAux
open FSharpAux.IO
open FSharpAux.IO.SchemaReader
open FSharpAux.IO.SchemaReader.Attribute


type UnionFormat = {
    Description: string
    Name: string
    NameSource : string
    Type: string option
    Amount: string
}

type Union = {
    Name : string
    Unions: UnionFormat []
}

let generateUnionTypeString (rootIdent:int) (format : Union) =
    let binding = sprintf "type %s = " format.Name
    let ident = [for i in [1 .. (4*rootIdent)] do yield " "] |> String.concat ""
    let body = 
        format.Unions
        |> Array.map (fun union ->  match union.Type with
                                    | Some t -> sprintf "///%s \r\n%s| %s of %s" union.Description ident union.Name t
                                    | _ -> sprintf "///%s \r\n%s| %s" union.Description ident union.Name
                                    )
    let additional = 
        sprintf "///Custom Attributes Used in the SOFT file\r\n%s|AdditionalAttribute of string*string" ident
    Array.concat [[|binding|];body;[|additional|]]
    |> Array.map (fun x -> sprintf "%s%s" ident x)
    |> String.concat "\r\n"


type SOFTSpecification = {
    [<FieldAttribute(0)>]
    FieldName : string
    [<FieldAttribute(1)>]
    Amount : string
    [<FieldAttribute(2)>]
    Type: string
    [<FieldAttribute(3)>]
    Description: string
}

let softSpecificationReader = Csv.CsvReader<SOFTSpecification>(SchemaMode = Csv.SchemaModes.Fill)

let seriesSpecPath = @"C:\Users\kevin\Downloads\CsbScaffold-master\MetaIndexing_New\data\SOFTSeriesSpecifications.txt"
let sampleSpecPath = @"C:\Users\kevin\Downloads\CsbScaffold-master\MetaIndexing_New\data\SOFTSampleSpecifications.txt"
let platformSpecPath = @"C:\Users\kevin\Downloads\CsbScaffold-master\MetaIndexing_New\data\SOFTPlatformSpecifications.txt"

let seriesSpec = 
    softSpecificationReader.ReadFile(seriesSpecPath, '\t', false) 
    |> Array.ofSeq
let sampleSpec =  
    softSpecificationReader.ReadFile(sampleSpecPath, '\t', false) 
    |> Array.ofSeq
let platformSpec =
    softSpecificationReader.ReadFile(platformSpecPath, '\t', false) 
    |> Array.ofSeq


let generateUnionType  (typeName:string) specs =
    specs
    |> Array.map (fun spec ->   let t = 
                                    if (spec.FieldName).Contains("[n]") && (spec.FieldName).Contains("list") then
                                        "int * (string list)"
                                    elif (spec.FieldName).Contains("[n]") then
                                        "int * string"
                                    else  
                                        "string"


                                {
                                    Description = spec.Description
                                    Name = 
                                        spec.FieldName
                                            .Remove(0,1)
                                            .Replace("_[n]", "")
                                            .Replace("_ch[n]", "")
                                            .Replace((typeName.ToUpper()), "Accession")
                                            .Replace((sprintf "%s_" typeName),"")
                                            .Split('_')
                                            |> fun x ->
                                                printfn "%A" x
                                                x
                                                |> Array.map 
                                                    (fun splits ->
                                                        printfn "%A" splits
                                                        let first = splits.[0].ToString().ToUpper()
                                                        sprintf "%s%s" first (splits.Substring(1))
                                                )
                                        |> Array.reduce (sprintf "%s%s")
                                    NameSource = spec.FieldName
                                    Type = Some t
                                    Amount = spec.Amount    
                                })
    |> (fun unions -> {Name = (sprintf "SOFT%sSpecifications" typeName); Unions = unions})

let (|IsMultiSpec|_|) (ident:string,name:string,u:UnionFormat) =
    match u.Type with
    | Some t when t = "int * string" -> 
        Some 
            (sprintf
            """
%s    if k.Contains("%s") then
%s        let index =
%s            k
%s                .Replace("%s","")
%s                .Replace("%s","") 
%s                |> int
%s        Some (%s.%s(index,v))
            """ 
            ident
            (u.NameSource
                .Replace("ch[n]","")
                .Replace("[n]",""))
            ident
            ident
            ident
            (u.NameSource
                .Replace("[n]","")
            )
            ident
            (u.NameSource
                .Replace("ch[n]",""))
            ident 
            ident
            name
            u.Name)
    | _ -> None

let (|IsMultiSpecList|_|) (ident:string,name:string,u:UnionFormat) =
    match u.Type with
    | Some t when t = "int * (string list)" -> 
        Some 
            (sprintf
            """
%s    if k.Contains("%s") then
%s        let index =
%s            k
%s                .Replace("%s","")
%s                .Replace("%s","") 
%s                |> int
%s        let keyList =
%s            v.Split(',') |> Array.toList
%s        Some (%s.%s(index,keyList))
%s            """ 
            ident
            (u.NameSource
                .Replace("ch[n]","")
                .Replace("[n]",""))
            ident
            ident
            ident
            (u.NameSource
                .Replace("[n]","")
            )
            ident
            (u.NameSource
                .Replace("ch[n]",""))
            ident
            ident
            ident
            ident
            name
            u.Name
            ident
            )
    | _ -> None

let generateActivePatterns (rootIdent:int) (union:Union) =
    let ident = [for i in [1 .. (4*rootIdent)] do yield " "] |> String.concat ""
    union.Unions
    |> Array.map 
        (fun u -> 
            let name = sprintf "let (|%s|_|) ((k:string),(v:string)) =" u.Name
            let body1 = 
                match (ident,union.Name,u) with
                |IsMultiSpec res    -> res
                |IsMultiSpecList res -> res
                |_ -> sprintf "    if k=\"%s\" then\r\n        %sSome(%s.%s v)" u.NameSource ident union.Name u.Name
            let body3 = sprintf "    else"
            let body4 = "        None"
            [name;body1;body3;body4]
            |> List.map (fun x -> sprintf "%s%s" ident x)
            |> String.concat "\r\n"
        )

let generateRecordType recordName (rootIdent:int) (union:Union) =
    let ident = [for i in [1 .. (4*rootIdent)] do yield " "] |> String.concat ""

    let verificationOfUnionFormat ident (u:UnionFormat) =
        match u.Amount with
        |"1" ->
            sprintf
                """
%s            |> listMustContainExactlyOne "%s must be exactly one value"
                """
                ident
                u.Name
        |"1 or more" ->
            sprintf
                """
%s            |> listMustContainOneOrMore "%s must be one or more values"
                """
                ident
                u.Name 
        | _ -> ""

    let recordBinding =
        sprintf "%stype %s = {" ident recordName 

    let recordFields =
        union.Unions
        |> Array.map 
            (fun u ->
                sprintf 
                    "%s    %s    : %s;"
                    ident
                    u.Name
                    (match u.Amount with
                    |"1" -> u.Type.Value
                    |_ -> sprintf "(%s) list" u.Type.Value)
            )

    let extractors =
        union.Unions
        |> Array.map 
            (fun u ->
                sprintf 
                    """
%s        %s =
%s            specList
%s            |> List.choose 
%s                (fun spec ->
%s                    match spec with
%s                    | %s.%s v -> Some v
%s                    | _ -> None
%s                )
            %s
                    """
                    ident
                    u.Name
                    ident
                    ident
                    ident
                    ident
                    ident
                    union.Name
                    u.Name
                    ident
                    ident
                    (verificationOfUnionFormat ident u)
            )
    [|
        [|recordBinding|]
        recordFields
        [|sprintf "%s}" ident|]
        [|sprintf "%slet create%s =" ident recordName|]
        [|sprintf "%s    {" ident|]
        extractors
        [|sprintf "%s}" ident|]
    |]
    |> Array.concat
    |> Array.filter (fun x -> not (x = ""))
    |> String.concat "\r\n"



let platformUnion           = generateUnionType "Platform" platformSpec
let platformUnionString     = platformUnion |> generateUnionTypeString 2
let platformActivePatterns  = platformUnion |> generateActivePatterns  3
let platformRecord          = platformUnion |> generateRecordType "PlatformRecord" 1

let seriesUnion = generateUnionType "Series" seriesSpec
let seriesUnionString     = seriesUnion |> generateUnionTypeString 2
let seriesActivePatterns  = seriesUnion |> generateActivePatterns 3
let seriesRecord          = seriesUnion |> generateRecordType "SeriesRecord" 1

let sampleUnion = generateUnionType "Sample" sampleSpec
let sampleUnionString     = sampleUnion |> generateUnionTypeString 2
let sampleActivePatterns  = sampleUnion |> generateActivePatterns 3
let sampleRecord          = sampleUnion |> generateRecordType "SampleRecord" 1


let generatorBody =
    sprintf
        """
module Generated =
    
    module Specifications =

%s

%s

%s

    module Lexing =
        
        open Specifications

        [<RequireQualifiedAccess>]
        module Platform =

%s
        [<RequireQualifiedAccess>]
        module Series =
%s
        [<RequireQualifiedAccess>]
        module Sample =
%s
        """
        platformUnionString
        seriesUnionString  
        sampleUnionString  

        (platformActivePatterns  |> String.concat "\r\n")
        (seriesActivePatterns    |> String.concat "\r\n")
        (sampleActivePatterns    |> String.concat "\r\n")
