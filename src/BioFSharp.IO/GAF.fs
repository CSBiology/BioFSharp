namespace BioFSharp.IO

open System
open FSharpAux
open FSharpAux.IO

module GAF =
    
    type GAFEntry = {
        Database            : string
        DbObjectID          : string
        DbObjectSymbol      : string
        Qualifier           : string []
        GoTerm              : string
        DbReference         : string []
        Evidence            : string
        WithFrom            : string []
        Aspect              : string
        DbObjectName        : string
        DbObjectSynonym     : string []
        DbObjectType        : string
        Taxon               : string []
        Date                : System.DateTime
        AssignedBy          : string
        AnnotationExtension : string [] option
        GeneProductFormId   : string option
                    }

    let createGAFEntry (str:string) version2 =  
        let split = str.Split([|'\t'|])
        { 
        Database            = split.[0]
        DbObjectID          = split.[1]
        DbObjectSymbol      = split.[2]
        Qualifier           = split.[3].Split([|'|'|])
        GoTerm              = split.[4]
        DbReference         = split.[5].Split([|'|'|])
        Evidence            = split.[6]
        WithFrom            = split.[7].Split([|'|'|])
        Aspect              = split.[8]
        DbObjectName        = split.[9]
        DbObjectSynonym     = split.[10].Split([|'|'|])
        DbObjectType        = split.[11]
        Taxon               = split.[12].Split([|'|'|])
        Date                = System.DateTime.ParseExact(split.[13],"yyyyMMdd",null).Date
        AssignedBy          = split.[14]
        AnnotationExtension = if version2 then Some (split.[15].Split([|','|])) else None
        GeneProductFormId   = if version2 then Some  split.[16]                 else None
        }
    
    type GAF = {
        Header  : seq<string>
        Entries : seq<GAFEntry>
        }

    let fromFile filepath :GAF=
        let strEnumerator = (FileIO.readFile(filepath)).GetEnumerator()

        let isVersion2 = 
            strEnumerator.MoveNext() |> ignore
            strEnumerator.Current.StartsWith("!gaf-version: 2")

        let rec parseSingle (accE:GAFEntry list) (accH:string list)=
            if strEnumerator.MoveNext() then 
                let currentString = strEnumerator.Current
                if currentString.StartsWith("!") then 
                    parseSingle accE (currentString::accH) 
                else 
                    parseSingle ((createGAFEntry currentString isVersion2)::accE) accH

            else 
                {Header =  accH |> List.rev |> Seq.cast
                 Entries = accE |> List.rev |> Seq.cast}   

        parseSingle [] []